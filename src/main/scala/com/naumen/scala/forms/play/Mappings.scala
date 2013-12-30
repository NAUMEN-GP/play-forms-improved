package com.naumen.scala.forms.play

import _root_.play.api.data.validation.{Valid, ValidationError, Invalid, Constraint}
import _root_.play.api.data._
import com.naumen.scala.utils.{FieldNameGetter, ClassConverter}
import scala._
import scala.collection.mutable

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
                val value = fieldValues.get(name).get
                mapping.withPrefix(name).unbind(value): (Map[String, String], Seq[FormError])

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
        constraints.map(_(t)).collect{
            case Invalid(errors) => errors.toSeq
        }.flatten.map{
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

//WARNING!!! Это копипаст плеевского Optional маппинга, c фиксом бага, при котором пробелы в опциональном поле проходят во Wrapped маппинг и вызывают срабатывание required-валидации
case class OptionalMapping_Fix[T](wrapped: Mapping[T], val constraints: Seq[Constraint[Option[T]]] = Nil) extends Mapping[Option[T]] {

    override val format: Option[(String, Seq[Any])] = wrapped.format

    /**
     * The field key.
     */
    val key = wrapped.key

    /**
     * Constructs a new Mapping based on this one, by adding new constraints.
     *
     * For example:
     * {{{
     *   import play.api.data._
     *   import validation.Constraints._
     *
     *   Form("phonenumber" -> text.verifying(required) )
     * }}}
     *
     * @param constraints the constraints to add
     * @return the new mapping
     */
    def verifying(addConstraints: Constraint[Option[T]]*): Mapping[Option[T]] = {
        this.copy(constraints = constraints ++ addConstraints.toSeq)
    }

    /**
     * Binds this field, i.e. constructs a concrete value from submitted data.
     *
     * @param data the submitted data
     * @return either a concrete value of type `T` or a set of error if the binding failed
     */
    def bind(data: Map[String, String]): Either[Seq[FormError], Option[T]] = {
        data.keys.filter(p => p == key || p.startsWith(key + ".") || p.startsWith(key + "[")).map(k => data.get(k).filterNot(_.trim.isEmpty)).collect { case Some(v) => v }.headOption.map { _ =>
            wrapped.bind(data).right.map(Some(_))
        }.getOrElse {
            Right(None)
        }.right.flatMap(applyConstraints)
    }

    /**
     * Unbinds this field, i.e. transforms a concrete value to plain data.
     *
     * @param value The value to unbind.
     * @return Either the plain data or a set of error if the unbinding failed.
     */
    def unbind(value: Option[T]): (Map[String, String], Seq[FormError]) = {
        val errors = collectErrors(value)
        value.map(wrapped.unbind(_)).map(r => r._1 -> (r._2 ++ errors)).getOrElse(Map.empty -> errors)
    }

    /**
     * Constructs a new Mapping based on this one, adding a prefix to the key.
     *
     * @param prefix the prefix to add to the key
     * @return the same mapping, with only the key changed
     */
    def withPrefix(prefix: String): Mapping[Option[T]] = {
        copy(wrapped = wrapped.withPrefix(prefix))
    }

    /** Sub-mappings (these can be seen as sub-keys). */
    val mappings: Seq[Mapping[_]] = wrapped.mappings

}

