package com.naumen.scala.forms.play

import _root_.play.api.data.validation._
import com.naumen.scala.forms._
import _root_.play.api.data._
import java.util.Date
import com.naumen.scala.utils.{FieldNameGetter, ClassConverter}
import scala._
import com.naumen.scala.forms.extensions.FieldExtensionsAttrs._
import scala.collection.mutable
import scala.Some
import com.naumen.scala.forms.FormDescriptionBuilder
import com.naumen.scala.forms.FormDescription

object PlayFormFactory {

    def form[T: Manifest](formFoo: FormDescriptionBuilder[T] => FormDescriptionBuilder[T], formConstraints: Constraint[T]*) = {
        produceForm(FormBuilderDsl.form[T](formFoo).build, formConstraints)
    }

    private def produceForm[T: Manifest](formDescription: FormDescription, formConstraints: Seq[Constraint[T]]) = {
        new ExtendedForm[T](
            fields = formDescription.fields.mapValues(makeField),
            filledForm = None,
            attrs = formDescription.attrs,
            formConstraints = formConstraints
        )
    }

    private def makeField(fieldDescription: FieldDescription) = {
        MappingFieldBuilder(
            fieldMapping = fieldMapping(fieldDescription),
            propertyMap = fieldDescription.propertyMap
        )
    }

    private def fieldMapping(implicit fieldDescription: FieldDescription) = {
        val fieldType = getClass(FieldDescription.FieldType)
        if (isSeqType(fieldType)) {
            listMapping
        } else {
            singularMapping
        }
    }

    private def singularMapping(implicit fieldDescription: FieldDescription) = {
        if (getBoolean(FieldDescription.Required)) {
            primitiveMapping
        } else {
            optionalMapping
        }
    }

    private def listMapping(implicit fieldDescription: FieldDescription) = {
        val elementType = getClass(FieldDescription.ListElementType)
        Forms.list(primitiveMapping(elementType))
    }

    private def optionalMapping(implicit fieldDescription: FieldDescription) = {
        Forms.optional(primitiveMapping)
    }

    private def primitiveMapping(implicit fieldDescription: FieldDescription): Mapping[_] = {
        val fieldType: Class[_] = getClass(FieldDescription.FieldType)
        primitiveMapping(fieldType)
    }

    private def isSeqType(fieldType: Class[_]) = {
        fieldType match {
            case ClassOfSeq | ClassOfList => true
            case _ => false

        }
    }

    private def primitiveMapping(clazz: Class[_])(implicit fieldDescription: FieldDescription): Mapping[_] = {

        val constraints = fieldDescription.propertyMap.get(PlayFieldProperties.Constraints).getOrElse(Nil).asInstanceOf[Seq[Constraint[Any]]]

        {
            val textMapping = Forms.text(getInt(MinLength), getIntOpt(MaxLength).getOrElse(Int.MaxValue))
            clazz match {
                case ClassOfString => if (getBoolean(FieldDescription.Required)) textMapping verifying Constraints.nonEmpty else textMapping
                case ClassOfInt => Forms.number
                case ClassOfBigDecimal => Forms.bigDecimal
                case ClassOfBoolean => Forms.boolean
                case ClassOfDate => {
                    val datePattern = getString(DateFormat)
                    Forms.date(datePattern)
                }
            }
        }.asInstanceOf[Mapping[Any]].verifying(constraints: _*)


    }


    val ClassOfString = classOf[String]
    val ClassOfInt = classOf[Int]
    val ClassOfBigDecimal = classOf[BigDecimal]
    val ClassOfBoolean = classOf[Boolean]
    val ClassOfDate = classOf[Date]
    val ClassOfSeq = classOf[Seq[_]]
    val ClassOfList = classOf[List[_]]

    private def getClass(key: String)(implicit fieldDescription: FieldDescription) = fieldDescription.propertyMap.get(key).get.asInstanceOf[Class[_]]

    private def getString(key: String)(implicit fieldDescription: FieldDescription) = fieldDescription.propertyMap.get(key).orNull.asInstanceOf[String]

    private def getInt(key: String)(implicit fieldDescription: FieldDescription) = fieldDescription.propertyMap.get(key).getOrElse(0).asInstanceOf[Int]

    private def getIntOpt(key: String)(implicit fieldDescription: FieldDescription) = fieldDescription.propertyMap.get(key).asInstanceOf[Option[Int]]

    private def getBoolean(key: String)(implicit fieldDescription: FieldDescription) = fieldDescription.propertyMap.get(key).getOrElse(false).asInstanceOf[Boolean]
}

case class MappingFieldBuilder[T](fieldMapping: Mapping[T], propertyMap: Map[String, Any]) extends Mapping[T] {
    type Self = MappingFieldBuilder[T]

    val key: String = fieldMapping.key
    val mappings: Seq[Mapping[_]] = fieldMapping.mappings
    val constraints: Seq[Constraint[T]] = fieldMapping.constraints

    def bind(data: Map[String, String]): Either[Seq[FormError], T] = fieldMapping.bind(data)

    def unbind(value: T): (Map[String, String], Seq[FormError]) = fieldMapping.unbind(value)

    def withPrefix(prefix: String): Self = copy(fieldMapping = fieldMapping.withPrefix(prefix))

    def verifying(constraints: Constraint[T]*): Self = copy(fieldMapping = fieldMapping.verifying(constraints: _*))
}

case class FormMapping[T: Manifest](fieldMappings: Map[String, Mapping[Any]], key: String = "", constraints: Seq[Constraint[T]] = Nil)
    extends Mapping[T] with ObjectMapping {
    def bind(data: Map[String, String]): Either[Seq[FormError], T] = {
        val (errors, values) = fieldMappings.map {
            case (name, mapping) => name -> mapping.withPrefix(name).bind(data)
        }.partition {
            case (_, either) => either.isLeft
        }

        if (errors.nonEmpty) {
            Left(errors.flatMap {
                case (_, leftError) => leftError.left.get
            }.toSeq)
        } else {
            val valuesMap = values.mapValues(_.right.get)
            applyConstraints(restoreEntity(valuesMap))
        }
    }

    def restoreEntity(valuesMap: Map[String, Any]): T = {
        val preparedMap = valuesMap.mapValues(_.asInstanceOf[AnyRef])
        ClassConverter.toInstanceOf[T](preparedMap)
    }

    def unbind(entity: T): (Map[String, String], Seq[FormError]) = {
        val fieldValues = toFieldValuesMap(entity)
        fieldMappings.map {
            case (name, mapping) =>
                val value: Any = fieldValues.get(name).get
                val unbound: (Map[String, String], Seq[FormError]) = mapping.withPrefix(name).unbind(value)
                unbound
        }.foldLeft((Map[String, String](), Seq[FormError]())) {
            case (a, (valueMap, errors)) => (a._1 ++ valueMap) -> (a._2 ++ errors)
        }
    }

    def toFieldValuesMap(entity: T): Map[String, Any] = {
        ClassConverter.toMap(entity.asInstanceOf[AnyRef])
    }

    def withPrefix(prefix: String): Mapping[T] = this.copy(key = addPrefix(prefix).getOrElse(key))

    def verifying(addConstraints: Constraint[T]*) = {
        this.copy(constraints = constraints ++ addConstraints.toSeq)
    }

    override protected def collectErrors(t: T): Seq[FormError] = {
        constraints.map(_(t)).collect {
            case Invalid(errors) => errors.toSeq
        }.flatten.map {
            case ve: ValidationErrorWithKey => FormError(ve.key, ve.message, ve.args)
            case ve: ValidationError => FormError(key, ve.message, ve.args)
        }

    }

    val mappings: scala.Seq[Mapping[_]] = Seq(this) ++ fieldMappings.map {
        case (name, mapping) => mapping
    }
}

class ValidationErrorWithKey(val key: String, wrappedError: ValidationError) extends ValidationError(wrappedError.message, wrappedError.args)

class ValidationResultBuilder[T: Manifest] extends FieldNameGetter {
    private val errors = mutable.Stack[ValidationErrorWithKey]()

    def addError(fieldFoo: T => Any)(message: String, args: Any*) =
        errors.push(new ValidationErrorWithKey($[T](fieldFoo), ValidationError(message, args)))

    def hasErrors = errors.isEmpty

    def validationResult = if (errors.isEmpty) Valid
    else Invalid.apply(errors.toList)


}

class ExtendedForm[T](fields: Map[String, MappingFieldBuilder[_]],
                      filledForm: Option[Form[T]],
                      val attrs: Map[String, Any],
                      formConstraints: Seq[Constraint[T]]
                         )(implicit private val mf: Manifest[T])
    extends Form[T](FormMapping[T](
        fieldMappings = fields.map {
            case (name, mfb) => name -> mfb.asInstanceOf[Mapping[Any]]
        }.toMap,
        constraints = formConstraints),
        filledForm.map(_.data).getOrElse(Map.empty[String, String]),
        filledForm.map(_.errors).getOrElse(Seq.empty[FormError]),
        filledForm.flatMap(_.value))
    with FieldNameGetter {

    def apply[F](foo: T => F): ExtendedField = apply($[T](foo))

    override def apply(key: String): ExtendedField = {
        val IndexedBrackets = "([^\\[\\]]+)\\[.*".r
        val Brackets = "([^\\[\\]]+)\\[\\]".r
        val (normalizedKey, valueOrMultiValue) =
            key match {
                case Brackets(normalKey) => {
                    normalKey -> Right(data.collect {
                        case (k, v) if k.startsWith(normalKey) => v
                    })
                }

                case k@IndexedBrackets(normalKey) => {
                    normalKey -> Left(data.get(key))
                }

                case _ => key -> Left(data.get(key))

            }



        val field: Field = Field(
            this,
            key,
            constraints.get(normalizedKey).getOrElse(Nil),
            formats.get(normalizedKey),
            errors.collect {
                case e if e.key == normalizedKey => e
            },
            valueOrMultiValue.left.getOrElse(valueOrMultiValue.right.toOption.flatMap(x => x.toSeq.headOption)))
        val fieldBuilderAttrs: Map[String, Any] = fields(normalizedKey).propertyMap

        val attrs: Map[String, Any] = fieldBuilderAttrs ++ valueOrMultiValue.right.toOption.map {
            mv =>
                Map("_customValue" -> mv)
        }.getOrElse(Map())

        new ExtendedField(this, field, new FieldExtension(attrs))
    }

    override def fill(value: T): ExtendedForm[T] = copy(filledForm = Some(super.fill(value)))

    override def bind(data: Map[String, String]): ExtendedForm[T] = mapping.bind(data).fold(
        newErrors => copy(filledForm = Some(Form[T](FormMapping[T](fields.map {
            case (name, mfb) => name -> mfb.asInstanceOf[Mapping[Any]]
        }.toMap), data, errors ++ newErrors, None))),
        value => copy(filledForm = Some(Form[T](FormMapping[T](fields.map {
            case (name, mfb) => name -> mfb.asInstanceOf[Mapping[Any]]
        }.toMap), data, Nil, Some(value))))
    )

    override def bindFromRequest()(implicit request: _root_.play.api.mvc.Request[_]): ExtendedForm[T] =
        super.bindFromRequest()(request).asInstanceOf[ExtendedForm[T]]

    def foldExtended[R](hasErrors: ExtendedForm[T] => R, success: T => R): R = value match {
        case Some(v) if errors.isEmpty => success(v)
        case _ => hasErrors(this)
    }

    override def withError(error: FormError): ExtendedForm[T] = copy(filledForm = Some(Form[T](FormMapping[T](fields.map {
        case (name, mfb) => name -> mfb.asInstanceOf[Mapping[Any]]
    }.toMap), data, errors :+ error, value)))

    private def copy(fields: Map[String, MappingFieldBuilder[_]] = fields, filledForm: Option[Form[T]] = None, attrs: Map[String, Any] = attrs) = {
        new ExtendedForm(fields, filledForm, attrs, formConstraints)
    }

}

case class FieldExtension(attrs: Map[String, Any])


class ExtendedField(val form: ExtendedForm[_], field: Field, val ext: FieldExtension)
    extends Field(form, field.name, field.constraints, field.format, field.errors, field.value) {

    override def apply(key: String): Field = {
        val key1: String = Option(name).filterNot(_.isEmpty).map(_ + (if (key(0) == '[') "" else ".")).getOrElse("") + key
        form(key1)
    }


}

object PlayFieldProperties {
    val Constraints = "play.constraints"
}
