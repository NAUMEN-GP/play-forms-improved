package com.naumen.scala.forms.play

import _root_.play.api.data.validation.Constraint
import com.naumen.scala.forms._
import _root_.play.api.data._
import java.util.Date
import com.naumen.scala.utils.ClassConverter
import scala._
import scala.Some
import com.naumen.scala.forms.FormDescriptionBuilder
import com.naumen.scala.forms.FormDescription
import com.naumen.scala.forms.extensions.FieldAttributes._

object PlayFormFactory {

  def form[T: Manifest](formFoo: FormDescriptionBuilder[T] => FormDescriptionBuilder[T]) = {
    produceForm(FormBuilderDsl.form[T](formFoo).build)
  }

  def produceForm[T: Manifest](formDescription: FormDescription) = {
    new ExtendedForm[T](fields = formDescription.fields.mapValues(makeField), attrs = formDescription.attrs)
  }

  def makeField(fieldDescription: FieldDescription) = {
    MappingFieldBuilder(
      fieldMapping = fieldMapping(fieldDescription),
      propertyMap = fieldDescription.propertyMap
    )
  }

  def fieldMapping(implicit fieldDescription: FieldDescription) = {
    val required = getBoolean(FieldDescription.Required)
    required match {
      case true => baseMappingBy(fieldDescription)
      case false => Forms.optional(baseMappingBy(fieldDescription))
    }
  }

  def baseMappingBy(implicit fieldDescription: FieldDescription) = {
    val fieldType = getClass(FieldDescription.FieldType)
    fieldType match {
      case ClassOfSeq | ClassOfList => {
        val elementType = getClass(FieldDescription.ListElementType)
        val mapping: Mapping[_] = elementaryMapping(elementType)
        Forms.list(mapping)
      }
      case _ => elementaryMapping(fieldType)
    }

  }

  def elementaryMapping(clazz: Class[_])(implicit fieldDescription: FieldDescription): Mapping[_] = {
    clazz match {
      case ClassOfString => Forms.text(getInt(MinLength), getIntOpt(MaxLength).getOrElse(Int.MaxValue))
      case ClassOfInt => Forms.number
      case ClassOfBoolean => Forms.boolean
      case ClassOfDate => {
        val datePattern = getString(DatePattern)
        Forms.date(datePattern)
      }
    }
  }


  val ClassOfString = classOf[String]
  val ClassOfInt = classOf[Int]
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
      case (name, mapping) => {
        name -> {
          val prefix: Mapping[Any] = mapping.withPrefix(name)
          val bind1: Either[Seq[FormError], Any] = prefix.bind(data)
          bind1
        }
      }
    }.partition {
      case (_, either) => either.isLeft
    }

    if (errors.nonEmpty) {
      Left(errors.flatMap {
        case (_, leftError) => leftError.left.get
      }.toSeq)
    } else {
      val valuesMap = values.mapValues(_.right.get)
      Right(restoreEntity(valuesMap))
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

  val mappings: scala.Seq[Mapping[_]] = Seq(this) ++ fieldMappings.map {
    case (name, mapping) => mapping
  }
}

class ExtendedForm[T](fields: Map[String, MappingFieldBuilder[_]], filledForm: Option[Form[T]] = None, val attrs: Map[String, Any] = Map())(implicit private val mf: Manifest[T])
  extends Form[T](
  FormMapping[T](fields.map {
    case (name, mfb) => name -> mfb.asInstanceOf[Mapping[Any]]
  }.toMap), {
    filledForm.map(_.data).getOrElse(Map.empty[String, String])
  },
  filledForm.map(_.errors).getOrElse(Seq.empty[FormError]),
  filledForm.flatMap(_.value)) {

  override def apply(key: String): Field = {
    val IndexedBrackets = "([^\\[\\]]+)\\[.*".r
    val Brackets = "([^\\[\\]]+)\\[\\]".r
    val (normalizedKey, valueOrMultiValue) =
      key match {
        case Brackets(normalKey) => {
          normalKey -> Right(data.collect {
            case (k, v) if k.startsWith(normalKey) => v
          })
        }

        case k @ IndexedBrackets(normalKey) => {
          normalKey -> Left(data.get(key))
        }

        case _ =>  key -> Left(data.get(key))

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
    }.getOrElse(Nil)

    new ExtendedField(this, field, new FieldExtension(attrs))
  }

  override def fill(value: T): Form[T] =
    new ExtendedForm(fields, Some(super.fill(value)), attrs)




}

case class FieldExtension(attrs: Map[String, Any])


class ExtendedField(val form: ExtendedForm[_], field: Field, val ext: FieldExtension)
  extends Field(form, field.name, field.constraints, field.format, field.errors, field.value) {

  override def apply(key: String): Field = {
    val key1: String = Option(name).filterNot(_.isEmpty).map(_ + (if (key(0) == '[') "" else ".")).getOrElse("") + key
    form(key1)
  }


}
