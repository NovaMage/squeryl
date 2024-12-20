/*******************************************************************************
 * Copyright 2010 Maxime Lévesque
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ***************************************************************************** */
package org.squeryl

import dsl.ast._
import dsl._

import java.util.{Date, UUID}
import java.sql.Timestamp
import java.time._
import org.squeryl.internals.{ArrayTEF, FieldMapper}

import java.sql

@deprecated(
  "the PrimitiveTypeMode companion object is deprecated, you should define a mix in the trait for your application. See : http://squeryl.org/0.9.6.html",
  "0.9.6"
)
object PrimitiveTypeMode extends PrimitiveTypeMode

private[squeryl] object InternalFieldMapper extends PrimitiveTypeMode

trait PrimitiveTypeMode extends QueryDsl with FieldMapper {

  // =========================== Non Numerical ===========================
  implicit val stringTEF: TypedExpressionFactory[String, TString] with PrimitiveJdbcMapper[String] =
    PrimitiveTypeSupport.stringTEF
  implicit val optionStringTEF: TypedExpressionFactory[Option[String], TOptionString]
    with DeOptionizer[String, String, TString, Option[String], TOptionString] = PrimitiveTypeSupport.optionStringTEF
  implicit val dateTEF: TypedExpressionFactory[Date, TDate] with PrimitiveJdbcMapper[Date] =
    PrimitiveTypeSupport.dateTEF
  implicit val optionDateTEF: TypedExpressionFactory[Option[Date], TOptionDate]
    with DeOptionizer[Date, Date, TDate, Option[Date], TOptionDate] = PrimitiveTypeSupport.optionDateTEF
  implicit val sqlDateTEF: TypedExpressionFactory[sql.Date, TDate] with PrimitiveJdbcMapper[sql.Date] =
    PrimitiveTypeSupport.sqlDateTEF
  implicit val optionSqlDateTEF: TypedExpressionFactory[Option[sql.Date], TOptionDate]
    with DeOptionizer[sql.Date, sql.Date, TDate, Option[sql.Date], TOptionDate] = PrimitiveTypeSupport.optionSqlDateTEF
  implicit val localDateTEF: TypedExpressionFactory[LocalDate, TLocalDate] with PrimitiveJdbcMapper[LocalDate] =
    PrimitiveTypeSupport.localDateTEF
  implicit val optionLocalDateTEF: TypedExpressionFactory[Option[LocalDate], TOptionLocalDate]
    with DeOptionizer[LocalDate, LocalDate, TLocalDate, Option[LocalDate], TOptionLocalDate] =
    PrimitiveTypeSupport.optionLocalDateTEF
  implicit val localTimeTEF: TypedExpressionFactory[LocalTime, TLocalTime] with PrimitiveJdbcMapper[LocalTime] =
    PrimitiveTypeSupport.localTimeTEF
  implicit val optionLocalTimeTEF: TypedExpressionFactory[Option[LocalTime], TOptionLocalTime]
    with DeOptionizer[LocalTime, LocalTime, TLocalTime, Option[LocalTime], TOptionLocalTime] =
    PrimitiveTypeSupport.optionLocalTimeTEF
  implicit val timestampTEF: TypedExpressionFactory[Timestamp, TTimestamp] with PrimitiveJdbcMapper[Timestamp] =
    PrimitiveTypeSupport.timestampTEF
  implicit val optionTimestampTEF: TypedExpressionFactory[Option[Timestamp], TOptionTimestamp]
    with DeOptionizer[Timestamp, Timestamp, TTimestamp, Option[Timestamp], TOptionTimestamp] =
    PrimitiveTypeSupport.optionTimestampTEF
  implicit val localDateTimeTEF
    : TypedExpressionFactory[LocalDateTime, TLocalDateTime] with PrimitiveJdbcMapper[LocalDateTime] =
    PrimitiveTypeSupport.localDateTimeTEF
  implicit val optionLocalDateTimeTEF: TypedExpressionFactory[Option[LocalDateTime], TOptionLocalDateTime]
    with DeOptionizer[LocalDateTime, LocalDateTime, TLocalDateTime, Option[LocalDateTime], TOptionLocalDateTime] =
    PrimitiveTypeSupport.optionLocalDateTimeTEF
  implicit val offsetTimeTEF: TypedExpressionFactory[OffsetTime, TOffsetTime] with PrimitiveJdbcMapper[OffsetTime] =
    PrimitiveTypeSupport.offsetTimeTEF
  implicit val optionOffsetTimeTEF: TypedExpressionFactory[Option[OffsetTime], TOptionOffsetTime]
    with DeOptionizer[OffsetTime, OffsetTime, TOffsetTime, Option[OffsetTime], TOptionOffsetTime] =
    PrimitiveTypeSupport.optionOffsetTimeTEF
  implicit val instantTEF: NonPrimitiveJdbcMapper[OffsetDateTime, Instant, TInstant] = PrimitiveTypeSupport.instantTEF
  implicit val optionInstantTEF: TypedExpressionFactory[Option[Instant], TOptionInstant]
    with DeOptionizer[OffsetDateTime, Instant, TInstant, Option[Instant], TOptionInstant] =
    PrimitiveTypeSupport.optionInstantTEF
  implicit val offsetDateTimeTEF
    : TypedExpressionFactory[OffsetDateTime, TOffsetDateTime] with PrimitiveJdbcMapper[OffsetDateTime] =
    PrimitiveTypeSupport.offsetDateTimeTEF
  implicit val optionOffsetDateTimeTEF: TypedExpressionFactory[Option[OffsetDateTime], TOptionOffsetDateTime]
    with DeOptionizer[OffsetDateTime, OffsetDateTime, TOffsetDateTime, Option[OffsetDateTime], TOptionOffsetDateTime] =
    PrimitiveTypeSupport.optionOffsetDateTimeTEF
  implicit val doubleArrayTEF: ArrayTEF[Double, TDoubleArray] = PrimitiveTypeSupport.doubleArrayTEF
  implicit val intArrayTEF: ArrayTEF[Int, TIntArray] = PrimitiveTypeSupport.intArrayTEF
  implicit val longArrayTEF: ArrayTEF[Long, TLongArray] = PrimitiveTypeSupport.longArrayTEF
  implicit val stringArrayTEF: ArrayTEF[String, TStringArray] = PrimitiveTypeSupport.stringArrayTEF

  // =========================== Numerical Integral ===========================
  implicit val byteTEF: IntegralTypedExpressionFactory[Byte, TByte, Float, TFloat] with PrimitiveJdbcMapper[Byte] =
    PrimitiveTypeSupport.byteTEF
  implicit val optionByteTEF: IntegralTypedExpressionFactory[Option[Byte], TOptionByte, Option[Float], TOptionFloat]
    with DeOptionizer[Byte, Byte, TByte, Option[Byte], TOptionByte] = PrimitiveTypeSupport.optionByteTEF
  implicit val intTEF: IntegralTypedExpressionFactory[Int, TInt, Float, TFloat] with PrimitiveJdbcMapper[Int] =
    PrimitiveTypeSupport.intTEF
  implicit val optionIntTEF: IntegralTypedExpressionFactory[Option[Int], TOptionInt, Option[Float], TOptionFloat]
    with DeOptionizer[Int, Int, TInt, Option[Int], TOptionInt] = PrimitiveTypeSupport.optionIntTEF
  implicit val longTEF: IntegralTypedExpressionFactory[Long, TLong, Double, TDouble] with PrimitiveJdbcMapper[Long] =
    PrimitiveTypeSupport.longTEF
  implicit val optionLongTEF: IntegralTypedExpressionFactory[Option[Long], TOptionLong, Option[Double], TOptionDouble]
    with DeOptionizer[Long, Long, TLong, Option[Long], TOptionLong] = PrimitiveTypeSupport.optionLongTEF

  // =========================== Numerical Floating Point ===========================
  implicit val floatTEF: FloatTypedExpressionFactory[Float, TFloat] with PrimitiveJdbcMapper[Float] =
    PrimitiveTypeSupport.floatTEF
  implicit val optionFloatTEF: FloatTypedExpressionFactory[Option[Float], TOptionFloat]
    with DeOptionizer[Float, Float, TFloat, Option[Float], TOptionFloat] = PrimitiveTypeSupport.optionFloatTEF
  implicit val doubleTEF: FloatTypedExpressionFactory[Double, TDouble] with PrimitiveJdbcMapper[Double] =
    PrimitiveTypeSupport.doubleTEF
  implicit val optionDoubleTEF: FloatTypedExpressionFactory[Option[Double], TOptionDouble]
    with DeOptionizer[Double, Double, TDouble, Option[Double], TOptionDouble] = PrimitiveTypeSupport.optionDoubleTEF
  implicit val bigDecimalTEF
    : FloatTypedExpressionFactory[BigDecimal, TBigDecimal] with PrimitiveJdbcMapper[BigDecimal] =
    PrimitiveTypeSupport.bigDecimalTEF
  implicit val optionBigDecimalTEF: FloatTypedExpressionFactory[Option[BigDecimal], TOptionBigDecimal]
    with DeOptionizer[BigDecimal, BigDecimal, TBigDecimal, Option[BigDecimal], TOptionBigDecimal] =
    PrimitiveTypeSupport.optionBigDecimalTEF

  implicit def stringToTE(s: String): TypedExpression[String, TString] = stringTEF.create(s)
  implicit def optionStringToTE(s: Option[String]): TypedExpression[Option[String], TOptionString] =
    optionStringTEF.create(s)

  implicit def dateToTE(s: Date): TypedExpression[Date, TDate] = dateTEF.create(s)
  implicit def optionDateToTE(s: Option[Date]): TypedExpression[Option[Date], TOptionDate] = optionDateTEF.create(s)

  implicit def localDateToTE(s: LocalDate): TypedExpression[LocalDate, TLocalDate] = localDateTEF.create(s)
  implicit def optionLocalDateToTE(s: Option[LocalDate]): TypedExpression[Option[LocalDate], TOptionLocalDate] =
    optionLocalDateTEF.create(s)

  implicit def localTimeToTE(s: LocalTime): TypedExpression[LocalTime, TLocalTime] = localTimeTEF.create(s)
  implicit def optionLocalTimeToTE(s: Option[LocalTime]): TypedExpression[Option[LocalTime], TOptionLocalTime] =
    optionLocalTimeTEF.create(s)

  implicit def timestampToTE(s: Timestamp): TypedExpression[Timestamp, TTimestamp] = timestampTEF.create(s)
  implicit def optionTimestampToTE(s: Option[Timestamp]): TypedExpression[Option[Timestamp], TOptionTimestamp] =
    optionTimestampTEF.create(s)

  implicit def localDateTimeToTE(s: LocalDateTime): TypedExpression[LocalDateTime, TLocalDateTime] =
    localDateTimeTEF.create(s)
  implicit def optionLocalDateTimeToTE(
    s: Option[LocalDateTime]
  ): TypedExpression[Option[LocalDateTime], TOptionLocalDateTime] = optionLocalDateTimeTEF.create(s)

  implicit def offsetTimeToTE(s: OffsetTime): TypedExpression[OffsetTime, TOffsetTime] = offsetTimeTEF.create(s)
  implicit def optionOffsetTimeToTE(s: Option[OffsetTime]): TypedExpression[Option[OffsetTime], TOptionOffsetTime] =
    optionOffsetTimeTEF.create(s)

  implicit def instantToTE(s: Instant): TypedExpression[Instant, TInstant] = instantTEF.create(s)

  implicit def optionInstantToTE(s: Option[Instant]): TypedExpression[Option[Instant], TOptionInstant] =
    optionInstantTEF.create(s)

  implicit def offsetDateTimeToTE(s: OffsetDateTime): TypedExpression[OffsetDateTime, TOffsetDateTime] =
    offsetDateTimeTEF.create(s)
  implicit def optionOffsetDateTimeToTE(
    s: Option[OffsetDateTime]
  ): TypedExpression[Option[OffsetDateTime], TOptionOffsetDateTime] = optionOffsetDateTimeTEF.create(s)

  implicit def zonedDateTimeToInstantTE(s: ZonedDateTime): TypedExpression[Instant, TInstant] =
    instantTEF.create(s.toInstant)
  implicit def optionZonedDateTimeToInstantTE(
    s: Some[ZonedDateTime]
  ): TypedExpression[Option[Instant], TOptionInstant] = optionInstantTEF.create(s.map(_.toInstant))

  implicit def booleanToTE(s: Boolean): TypedExpression[Boolean, TBoolean] = PrimitiveTypeSupport.booleanTEF.create(s)
  implicit def optionBooleanToTE(s: Option[Boolean]): TypedExpression[Option[Boolean], TOptionBoolean] =
    PrimitiveTypeSupport.optionBooleanTEF.create(s)

  implicit def uuidToTE(s: UUID): TypedExpression[UUID, TUUID] = PrimitiveTypeSupport.uuidTEF.create(s)
  implicit def optionUUIDToTE(s: Option[UUID]): TypedExpression[Option[UUID], TOptionUUID] =
    PrimitiveTypeSupport.optionUUIDTEF.create(s)

  implicit def binaryToTE(s: Array[Byte]): TypedExpression[Array[Byte], TByteArray] =
    PrimitiveTypeSupport.binaryTEF.create(s)
  implicit def optionByteArrayToTE(s: Option[Array[Byte]]): TypedExpression[Option[Array[Byte]], TOptionByteArray] =
    PrimitiveTypeSupport.optionByteArrayTEF.create(s)

  implicit def enumValueToTE[A >: Enumeration#Value <: Enumeration#Value](e: A): TypedExpression[A, TEnumValue[A]] =
    PrimitiveTypeSupport.enumValueTEF[A](e).create(e)

  implicit def optionEnumcValueToTE[A >: Enumeration#Value <: Enumeration#Value](
    e: Option[A]
  ): TypedExpression[Option[A], TOptionEnumValue[A]] =
    PrimitiveTypeSupport.optionEnumValueTEF[A](e).create(e)

  implicit def byteToTE(f: Byte): TypedExpression[Byte, TByte] = byteTEF.create(f)
  implicit def optionByteToTE(f: Option[Byte]): TypedExpression[Option[Byte], TOptionByte] = optionByteTEF.create(f)

  implicit def intToTE(f: Int): TypedExpression[Int, TInt] = intTEF.create(f)
  implicit def optionIntToTE(f: Option[Int]): TypedExpression[Option[Int], TOptionInt] = optionIntTEF.create(f)

  implicit def longToTE(f: Long): TypedExpression[Long, TLong] = longTEF.create(f)
  implicit def optionLongToTE(f: Option[Long]): TypedExpression[Option[Long], TOptionLong] = optionLongTEF.create(f)

  implicit def floatToTE(f: Float): TypedExpression[Float, TFloat] = floatTEF.create(f)
  implicit def optionFloatToTE(f: Option[Float]): TypedExpression[Option[Float], TOptionFloat] =
    optionFloatTEF.create(f)

  implicit def doubleToTE(f: Double): TypedExpression[Double, TDouble] = doubleTEF.create(f)
  implicit def optionDoubleToTE(f: Option[Double]): TypedExpression[Option[Double], TOptionDouble] =
    optionDoubleTEF.create(f)

  implicit def bigDecimalToTE(f: BigDecimal): TypedExpression[BigDecimal, TBigDecimal] = bigDecimalTEF.create(f)
  implicit def optionBigDecimalToTE(f: Option[BigDecimal]): TypedExpression[Option[BigDecimal], TOptionBigDecimal] =
    optionBigDecimalTEF.create(f)

  implicit def doubleArrayToTE(f: Array[Double]): TypedExpression[Array[Double], TDoubleArray] =
    doubleArrayTEF.create(f)
  implicit def intArrayToTE(f: Array[Int]): TypedExpression[Array[Int], TIntArray] = intArrayTEF.create(f)
  implicit def longArrayToTE(f: Array[Long]): TypedExpression[Array[Long], TLongArray] = longArrayTEF.create(f)
  implicit def stringArrayToTE(f: Array[String]): TypedExpression[Array[String], TStringArray] =
    stringArrayTEF.create(f)

  implicit def logicalBooleanToTE(l: LogicalBoolean): TypedExpressionConversion[Boolean, TBoolean] =
    PrimitiveTypeSupport.booleanTEF.convert(l)

  implicit def queryStringToTE(q: Query[String]): QueryValueExpressionNode[String, TString] =
    new QueryValueExpressionNode[String, TString](q.copy(false, Nil).ast, stringTEF.createOutMapper)
  implicit def queryOptionStringToTE(
    q: Query[Option[String]]
  ): QueryValueExpressionNode[Option[String], TOptionString] =
    new QueryValueExpressionNode[Option[String], TOptionString](q.copy(false, Nil).ast, optionStringTEF.createOutMapper)
  implicit def queryStringGroupedToTE(q: Query[Group[String]]): QueryValueExpressionNode[String, TString] =
    new QueryValueExpressionNode[String, TString](q.copy(false, Nil).ast, stringTEF.createOutMapper)
  implicit def queryOptionStringGroupedToTE(
    q: Query[Group[Option[String]]]
  ): QueryValueExpressionNode[Option[String], TOptionString] =
    new QueryValueExpressionNode[Option[String], TOptionString](q.copy(false, Nil).ast, optionStringTEF.createOutMapper)
  implicit def queryStringMeasuredToTE(q: Query[Measures[String]]): QueryValueExpressionNode[String, TString] =
    new QueryValueExpressionNode[String, TString](q.copy(false, Nil).ast, stringTEF.createOutMapper)
  implicit def queryOptionStringMeasuredToTE(
    q: Query[Measures[Option[String]]]
  ): QueryValueExpressionNode[Option[String], TOptionString] =
    new QueryValueExpressionNode[Option[String], TOptionString](q.copy(false, Nil).ast, optionStringTEF.createOutMapper)

  implicit def queryDateToTE(q: Query[Date]): QueryValueExpressionNode[Date, TDate] =
    new QueryValueExpressionNode[Date, TDate](q.copy(false, Nil).ast, dateTEF.createOutMapper)
  implicit def queryOptionDateToTE(q: Query[Option[Date]]): QueryValueExpressionNode[Option[Date], TOptionDate] =
    new QueryValueExpressionNode[Option[Date], TOptionDate](q.copy(false, Nil).ast, optionDateTEF.createOutMapper)
  implicit def queryDateGroupedToTE(q: Query[Group[Date]]): QueryValueExpressionNode[Date, TDate] =
    new QueryValueExpressionNode[Date, TDate](q.copy(false, Nil).ast, dateTEF.createOutMapper)
  implicit def queryOptionDateGroupedToTE(
    q: Query[Group[Option[Date]]]
  ): QueryValueExpressionNode[Option[Date], TOptionDate] =
    new QueryValueExpressionNode[Option[Date], TOptionDate](q.copy(false, Nil).ast, optionDateTEF.createOutMapper)
  implicit def queryDateMeasuredToTE(q: Query[Measures[Date]]): QueryValueExpressionNode[Date, TDate] =
    new QueryValueExpressionNode[Date, TDate](q.copy(false, Nil).ast, dateTEF.createOutMapper)
  implicit def queryOptionDateMeasuredToTE(
    q: Query[Measures[Option[Date]]]
  ): QueryValueExpressionNode[Option[Date], TOptionDate] =
    new QueryValueExpressionNode[Option[Date], TOptionDate](q.copy(false, Nil).ast, optionDateTEF.createOutMapper)

  implicit def queryLocalDateToTE(q: Query[LocalDate]): QueryValueExpressionNode[LocalDate, TLocalDate] =
    new QueryValueExpressionNode[LocalDate, TLocalDate](q.copy(false, Nil).ast, localDateTEF.createOutMapper)
  implicit def queryOptionLocalDateToTE(
    q: Query[Option[LocalDate]]
  ): QueryValueExpressionNode[Option[LocalDate], TOptionLocalDate] =
    new QueryValueExpressionNode[Option[LocalDate], TOptionLocalDate](
      q.copy(false, Nil).ast,
      optionLocalDateTEF.createOutMapper
    )
  implicit def queryLocalDateGroupedToTE(q: Query[Group[LocalDate]]): QueryValueExpressionNode[LocalDate, TLocalDate] =
    new QueryValueExpressionNode[LocalDate, TLocalDate](q.copy(false, Nil).ast, localDateTEF.createOutMapper)
  implicit def queryOptionLocalDateGroupedToTE(
    q: Query[Group[Option[LocalDate]]]
  ): QueryValueExpressionNode[Option[LocalDate], TOptionLocalDate] =
    new QueryValueExpressionNode[Option[LocalDate], TOptionLocalDate](
      q.copy(false, Nil).ast,
      optionLocalDateTEF.createOutMapper
    )
  implicit def queryLocalDateMeasuredToTE(
    q: Query[Measures[LocalDate]]
  ): QueryValueExpressionNode[LocalDate, TLocalDate] =
    new QueryValueExpressionNode[LocalDate, TLocalDate](q.copy(false, Nil).ast, localDateTEF.createOutMapper)
  implicit def queryOptionLocalDateMeasuredToTE(
    q: Query[Measures[Option[LocalDate]]]
  ): QueryValueExpressionNode[Option[LocalDate], TOptionLocalDate] =
    new QueryValueExpressionNode[Option[LocalDate], TOptionLocalDate](
      q.copy(false, Nil).ast,
      optionLocalDateTEF.createOutMapper
    )

  implicit def queryLocalTimeToTE(q: Query[LocalTime]): QueryValueExpressionNode[LocalTime, TLocalTime] =
    new QueryValueExpressionNode[LocalTime, TLocalTime](q.copy(false, Nil).ast, localTimeTEF.createOutMapper)
  implicit def queryOptionLocalTimeToTE(
    q: Query[Option[LocalTime]]
  ): QueryValueExpressionNode[Option[LocalTime], TOptionLocalTime] =
    new QueryValueExpressionNode[Option[LocalTime], TOptionLocalTime](
      q.copy(false, Nil).ast,
      optionLocalTimeTEF.createOutMapper
    )
  implicit def queryLocalTimeGroupedToTE(q: Query[Group[LocalTime]]): QueryValueExpressionNode[LocalTime, TLocalTime] =
    new QueryValueExpressionNode[LocalTime, TLocalTime](q.copy(false, Nil).ast, localTimeTEF.createOutMapper)
  implicit def queryOptionLocalTimeGroupedToTE(
    q: Query[Group[Option[LocalTime]]]
  ): QueryValueExpressionNode[Option[LocalTime], TOptionLocalTime] =
    new QueryValueExpressionNode[Option[LocalTime], TOptionLocalTime](
      q.copy(false, Nil).ast,
      optionLocalTimeTEF.createOutMapper
    )
  implicit def queryLocalTimeMeasuredToTE(
    q: Query[Measures[LocalTime]]
  ): QueryValueExpressionNode[LocalTime, TLocalTime] =
    new QueryValueExpressionNode[LocalTime, TLocalTime](q.copy(false, Nil).ast, localTimeTEF.createOutMapper)
  implicit def queryOptionLocalTimeMeasuredToTE(
    q: Query[Measures[Option[LocalTime]]]
  ): QueryValueExpressionNode[Option[LocalTime], TOptionLocalTime] =
    new QueryValueExpressionNode[Option[LocalTime], TOptionLocalTime](
      q.copy(false, Nil).ast,
      optionLocalTimeTEF.createOutMapper
    )

  implicit def queryTimestampToTE(q: Query[Timestamp]): QueryValueExpressionNode[Timestamp, TTimestamp] =
    new QueryValueExpressionNode[Timestamp, TTimestamp](q.copy(false, Nil).ast, timestampTEF.createOutMapper)
  implicit def queryOptionTimestampToTE(
    q: Query[Option[Timestamp]]
  ): QueryValueExpressionNode[Option[Timestamp], TOptionTimestamp] =
    new QueryValueExpressionNode[Option[Timestamp], TOptionTimestamp](
      q.copy(false, Nil).ast,
      optionTimestampTEF.createOutMapper
    )
  implicit def queryTimestampGroupedToTE(q: Query[Group[Timestamp]]): QueryValueExpressionNode[Timestamp, TTimestamp] =
    new QueryValueExpressionNode[Timestamp, TTimestamp](q.copy(false, Nil).ast, timestampTEF.createOutMapper)
  implicit def queryOptionTimestampGroupedToTE(
    q: Query[Group[Option[Timestamp]]]
  ): QueryValueExpressionNode[Option[Timestamp], TOptionTimestamp] =
    new QueryValueExpressionNode[Option[Timestamp], TOptionTimestamp](
      q.copy(false, Nil).ast,
      optionTimestampTEF.createOutMapper
    )
  implicit def queryTimestampMeasuredToTE(
    q: Query[Measures[Timestamp]]
  ): QueryValueExpressionNode[Timestamp, TTimestamp] =
    new QueryValueExpressionNode[Timestamp, TTimestamp](q.copy(false, Nil).ast, timestampTEF.createOutMapper)
  implicit def queryOptionTimestampMeasuredToTE(
    q: Query[Measures[Option[Timestamp]]]
  ): QueryValueExpressionNode[Option[Timestamp], TOptionTimestamp] =
    new QueryValueExpressionNode[Option[Timestamp], TOptionTimestamp](
      q.copy(false, Nil).ast,
      optionTimestampTEF.createOutMapper
    )

  implicit def queryLocalDateTimeToTE(
    q: Query[LocalDateTime]
  ): QueryValueExpressionNode[LocalDateTime, TLocalDateTime] =
    new QueryValueExpressionNode[LocalDateTime, TLocalDateTime](
      q.copy(false, Nil).ast,
      localDateTimeTEF.createOutMapper
    )
  implicit def queryOptionLocalDateTimeToTE(
    q: Query[Option[LocalDateTime]]
  ): QueryValueExpressionNode[Option[LocalDateTime], TOptionLocalDateTime] =
    new QueryValueExpressionNode[Option[LocalDateTime], TOptionLocalDateTime](
      q.copy(false, Nil).ast,
      optionLocalDateTimeTEF.createOutMapper
    )
  implicit def queryLocalDateTimeGroupedToTE(
    q: Query[Group[LocalDateTime]]
  ): QueryValueExpressionNode[LocalDateTime, TLocalDateTime] =
    new QueryValueExpressionNode[LocalDateTime, TLocalDateTime](
      q.copy(false, Nil).ast,
      localDateTimeTEF.createOutMapper
    )
  implicit def queryOptionLocalDateTimeGroupedToTE(
    q: Query[Group[Option[LocalDateTime]]]
  ): QueryValueExpressionNode[Option[LocalDateTime], TOptionLocalDateTime] =
    new QueryValueExpressionNode[Option[LocalDateTime], TOptionLocalDateTime](
      q.copy(false, Nil).ast,
      optionLocalDateTimeTEF.createOutMapper
    )
  implicit def queryLocalDateTimeMeasuredToTE(
    q: Query[Measures[LocalDateTime]]
  ): QueryValueExpressionNode[LocalDateTime, TLocalDateTime] =
    new QueryValueExpressionNode[LocalDateTime, TLocalDateTime](
      q.copy(false, Nil).ast,
      localDateTimeTEF.createOutMapper
    )
  implicit def queryOptionLocalDateTimeMeasuredToTE(
    q: Query[Measures[Option[LocalDateTime]]]
  ): QueryValueExpressionNode[Option[LocalDateTime], TOptionLocalDateTime] =
    new QueryValueExpressionNode[Option[LocalDateTime], TOptionLocalDateTime](
      q.copy(false, Nil).ast,
      optionLocalDateTimeTEF.createOutMapper
    )

  implicit def queryOffsetTimeToTE(q: Query[OffsetTime]): QueryValueExpressionNode[OffsetTime, TOffsetTime] =
    new QueryValueExpressionNode[OffsetTime, TOffsetTime](q.copy(false, Nil).ast, offsetTimeTEF.createOutMapper)
  implicit def queryOptionOffsetTimeToTE(
    q: Query[Option[OffsetTime]]
  ): QueryValueExpressionNode[Option[OffsetTime], TOptionOffsetTime] =
    new QueryValueExpressionNode[Option[OffsetTime], TOptionOffsetTime](
      q.copy(false, Nil).ast,
      optionOffsetTimeTEF.createOutMapper
    )
  implicit def queryOffsetTimeGroupedToTE(
    q: Query[Group[OffsetTime]]
  ): QueryValueExpressionNode[OffsetTime, TOffsetTime] =
    new QueryValueExpressionNode[OffsetTime, TOffsetTime](q.copy(false, Nil).ast, offsetTimeTEF.createOutMapper)
  implicit def queryOptionOffsetTimeGroupedToTE(
    q: Query[Group[Option[OffsetTime]]]
  ): QueryValueExpressionNode[Option[OffsetTime], TOptionOffsetTime] =
    new QueryValueExpressionNode[Option[OffsetTime], TOptionOffsetTime](
      q.copy(false, Nil).ast,
      optionOffsetTimeTEF.createOutMapper
    )
  implicit def queryOffsetTimeMeasuredToTE(
    q: Query[Measures[OffsetTime]]
  ): QueryValueExpressionNode[OffsetTime, TOffsetTime] =
    new QueryValueExpressionNode[OffsetTime, TOffsetTime](q.copy(false, Nil).ast, offsetTimeTEF.createOutMapper)
  implicit def queryOptionOffsetTimeMeasuredToTE(
    q: Query[Measures[Option[OffsetTime]]]
  ): QueryValueExpressionNode[Option[OffsetTime], TOptionOffsetTime] =
    new QueryValueExpressionNode[Option[OffsetTime], TOptionOffsetTime](
      q.copy(false, Nil).ast,
      optionOffsetTimeTEF.createOutMapper
    )

  implicit def queryInstantToTE(q: Query[Instant]): QueryValueExpressionNode[Instant, TInstant] =
    new QueryValueExpressionNode[Instant, TInstant](q.copy(false, Nil).ast, instantTEF.createOutMapper)

  implicit def queryOptionInstantToTE(
    q: Query[Option[Instant]]
  ): QueryValueExpressionNode[Option[Instant], TOptionInstant] =
    new QueryValueExpressionNode[Option[Instant], TOptionInstant](
      q.copy(false, Nil).ast,
      optionInstantTEF.createOutMapper
    )

  implicit def queryInstantGroupedToTE(q: Query[Group[Instant]]): QueryValueExpressionNode[Instant, TInstant] =
    new QueryValueExpressionNode[Instant, TInstant](q.copy(false, Nil).ast, instantTEF.createOutMapper)

  implicit def queryOptionInstantGroupedToTE(
    q: Query[Group[Option[Instant]]]
  ): QueryValueExpressionNode[Option[Instant], TOptionInstant] =
    new QueryValueExpressionNode[Option[Instant], TOptionInstant](
      q.copy(false, Nil).ast,
      optionInstantTEF.createOutMapper
    )

  implicit def queryInstantMeasuredToTE(q: Query[Measures[Instant]]): QueryValueExpressionNode[Instant, TInstant] =
    new QueryValueExpressionNode[Instant, TInstant](q.copy(false, Nil).ast, instantTEF.createOutMapper)

  implicit def queryOptionInstantMeasuredToTE(
    q: Query[Measures[Option[Instant]]]
  ): QueryValueExpressionNode[Option[Instant], TOptionInstant] =
    new QueryValueExpressionNode[Option[Instant], TOptionInstant](
      q.copy(false, Nil).ast,
      optionInstantTEF.createOutMapper
    )

  implicit def queryOffsetDateTimeToTE(
    q: Query[OffsetDateTime]
  ): QueryValueExpressionNode[OffsetDateTime, TOffsetDateTime] =
    new QueryValueExpressionNode[OffsetDateTime, TOffsetDateTime](
      q.copy(false, Nil).ast,
      offsetDateTimeTEF.createOutMapper
    )
  implicit def queryOptionOffsetDateTimeToTE(
    q: Query[Option[OffsetDateTime]]
  ): QueryValueExpressionNode[Option[OffsetDateTime], TOptionOffsetDateTime] =
    new QueryValueExpressionNode[Option[OffsetDateTime], TOptionOffsetDateTime](
      q.copy(false, Nil).ast,
      optionOffsetDateTimeTEF.createOutMapper
    )
  implicit def queryOffsetDateTimeGroupedToTE(
    q: Query[Group[OffsetDateTime]]
  ): QueryValueExpressionNode[OffsetDateTime, TOffsetDateTime] =
    new QueryValueExpressionNode[OffsetDateTime, TOffsetDateTime](
      q.copy(false, Nil).ast,
      offsetDateTimeTEF.createOutMapper
    )
  implicit def queryOptionOffsetDateTimeGroupedToTE(
    q: Query[Group[Option[OffsetDateTime]]]
  ): QueryValueExpressionNode[Option[OffsetDateTime], TOptionOffsetDateTime] =
    new QueryValueExpressionNode[Option[OffsetDateTime], TOptionOffsetDateTime](
      q.copy(false, Nil).ast,
      optionOffsetDateTimeTEF.createOutMapper
    )
  implicit def queryOffsetDateTimeMeasuredToTE(
    q: Query[Measures[OffsetDateTime]]
  ): QueryValueExpressionNode[OffsetDateTime, TOffsetDateTime] =
    new QueryValueExpressionNode[OffsetDateTime, TOffsetDateTime](
      q.copy(false, Nil).ast,
      offsetDateTimeTEF.createOutMapper
    )
  implicit def queryOptionOffsetDateTimeMeasuredToTE(
    q: Query[Measures[Option[OffsetDateTime]]]
  ): QueryValueExpressionNode[Option[OffsetDateTime], TOptionOffsetDateTime] =
    new QueryValueExpressionNode[Option[OffsetDateTime], TOptionOffsetDateTime](
      q.copy(false, Nil).ast,
      optionOffsetDateTimeTEF.createOutMapper
    )

  implicit def queryBooleanToTE(q: Query[Boolean]): QueryValueExpressionNode[Boolean, TBoolean] =
    new QueryValueExpressionNode[Boolean, TBoolean](
      q.copy(false, Nil).ast,
      PrimitiveTypeSupport.booleanTEF.createOutMapper
    )
  implicit def queryOptionBooleanToTE(
    q: Query[Option[Boolean]]
  ): QueryValueExpressionNode[Option[Boolean], TOptionBoolean] =
    new QueryValueExpressionNode[Option[Boolean], TOptionBoolean](
      q.copy(false, Nil).ast,
      PrimitiveTypeSupport.optionBooleanTEF.createOutMapper
    )

  implicit def queryUUIDToTE(q: Query[UUID]): QueryValueExpressionNode[UUID, TUUID] =
    new QueryValueExpressionNode[UUID, TUUID](q.copy(false, Nil).ast, PrimitiveTypeSupport.uuidTEF.createOutMapper)
  implicit def queryOptionUUIDToTE(q: Query[Option[UUID]]): QueryValueExpressionNode[Option[UUID], TOptionUUID] =
    new QueryValueExpressionNode[Option[UUID], TOptionUUID](
      q.copy(false, Nil).ast,
      PrimitiveTypeSupport.optionUUIDTEF.createOutMapper
    )

  implicit def queryByteArrayToTE(q: Query[Array[Byte]]): QueryValueExpressionNode[Array[Byte], TByteArray] =
    new QueryValueExpressionNode[Array[Byte], TByteArray](
      q.copy(false, Nil).ast,
      PrimitiveTypeSupport.binaryTEF.createOutMapper
    )
  implicit def queryOptionByteArrayToTE(
    q: Query[Option[Array[Byte]]]
  ): QueryValueExpressionNode[Option[Array[Byte]], TOptionByteArray] =
    new QueryValueExpressionNode[Option[Array[Byte]], TOptionByteArray](
      q.copy(false, Nil).ast,
      PrimitiveTypeSupport.optionByteArrayTEF.createOutMapper
    )

  implicit def queryByteToTE(q: Query[Byte]): QueryValueExpressionNode[Byte, TByte] =
    new QueryValueExpressionNode[Byte, TByte](q.copy(false, Nil).ast, byteTEF.createOutMapper)
  implicit def queryOptionByteToTE(q: Query[Option[Byte]]): QueryValueExpressionNode[Option[Byte], TOptionByte] =
    new QueryValueExpressionNode[Option[Byte], TOptionByte](q.copy(false, Nil).ast, optionByteTEF.createOutMapper)
  implicit def queryByteGroupedToTE(q: Query[Group[Byte]]): QueryValueExpressionNode[Byte, TByte] =
    new QueryValueExpressionNode[Byte, TByte](q.copy(false, Nil).ast, byteTEF.createOutMapper)
  implicit def queryOptionByteGroupedToTE(
    q: Query[Group[Option[Byte]]]
  ): QueryValueExpressionNode[Option[Byte], TOptionByte] =
    new QueryValueExpressionNode[Option[Byte], TOptionByte](q.copy(false, Nil).ast, optionByteTEF.createOutMapper)
  implicit def queryByteMeasuredToTE(q: Query[Measures[Byte]]): QueryValueExpressionNode[Byte, TByte] =
    new QueryValueExpressionNode[Byte, TByte](q.copy(false, Nil).ast, byteTEF.createOutMapper)
  implicit def queryOptionByteMeasuredToTE(
    q: Query[Measures[Option[Byte]]]
  ): QueryValueExpressionNode[Option[Byte], TOptionByte] =
    new QueryValueExpressionNode[Option[Byte], TOptionByte](q.copy(false, Nil).ast, optionByteTEF.createOutMapper)

  implicit def queryIntToTE(q: Query[Int]): QueryValueExpressionNode[Int, TInt] =
    new QueryValueExpressionNode[Int, TInt](q.copy(false, Nil).ast, intTEF.createOutMapper)
  implicit def queryOptionIntToTE(q: Query[Option[Int]]): QueryValueExpressionNode[Option[Int], TOptionInt] =
    new QueryValueExpressionNode[Option[Int], TOptionInt](q.copy(false, Nil).ast, optionIntTEF.createOutMapper)
  implicit def queryIntGroupedToTE(q: Query[Group[Int]]): QueryValueExpressionNode[Int, TInt] =
    new QueryValueExpressionNode[Int, TInt](q.copy(false, Nil).ast, intTEF.createOutMapper)
  implicit def queryOptionIntGroupedToTE(
    q: Query[Group[Option[Int]]]
  ): QueryValueExpressionNode[Option[Int], TOptionInt] =
    new QueryValueExpressionNode[Option[Int], TOptionInt](q.copy(false, Nil).ast, optionIntTEF.createOutMapper)
  implicit def queryIntMeasuredToTE(q: Query[Measures[Int]]): QueryValueExpressionNode[Int, TInt] =
    new QueryValueExpressionNode[Int, TInt](q.copy(false, Nil).ast, intTEF.createOutMapper)
  implicit def queryOptionIntMeasuredToTE(
    q: Query[Measures[Option[Int]]]
  ): QueryValueExpressionNode[Option[Int], TOptionInt] =
    new QueryValueExpressionNode[Option[Int], TOptionInt](q.copy(false, Nil).ast, optionIntTEF.createOutMapper)

  implicit def queryLongToTE(q: Query[Long]): QueryValueExpressionNode[Long, TLong] =
    new QueryValueExpressionNode[Long, TLong](q.copy(false, Nil).ast, longTEF.createOutMapper)
  implicit def queryOptionLongToTE(q: Query[Option[Long]]): QueryValueExpressionNode[Option[Long], TOptionLong] =
    new QueryValueExpressionNode[Option[Long], TOptionLong](q.copy(false, Nil).ast, optionLongTEF.createOutMapper)
  implicit def queryLongGroupedToTE(q: Query[Group[Long]]): QueryValueExpressionNode[Long, TLong] =
    new QueryValueExpressionNode[Long, TLong](q.copy(false, Nil).ast, longTEF.createOutMapper)
  implicit def queryOptionLongGroupedToTE(
    q: Query[Group[Option[Long]]]
  ): QueryValueExpressionNode[Option[Long], TOptionLong] =
    new QueryValueExpressionNode[Option[Long], TOptionLong](q.copy(false, Nil).ast, optionLongTEF.createOutMapper)
  implicit def queryLongMeasuredToTE(q: Query[Measures[Long]]): QueryValueExpressionNode[Long, TLong] =
    new QueryValueExpressionNode[Long, TLong](q.copy(false, Nil).ast, longTEF.createOutMapper)
  implicit def queryOptionLongMeasuredToTE(
    q: Query[Measures[Option[Long]]]
  ): QueryValueExpressionNode[Option[Long], TOptionLong] =
    new QueryValueExpressionNode[Option[Long], TOptionLong](q.copy(false, Nil).ast, optionLongTEF.createOutMapper)

  implicit def queryFloatToTE(q: Query[Float]): QueryValueExpressionNode[Float, TFloat] =
    new QueryValueExpressionNode[Float, TFloat](q.copy(false, Nil).ast, floatTEF.createOutMapper)
  implicit def queryOptionFloatToTE(q: Query[Option[Float]]): QueryValueExpressionNode[Option[Float], TOptionFloat] =
    new QueryValueExpressionNode[Option[Float], TOptionFloat](q.copy(false, Nil).ast, optionFloatTEF.createOutMapper)
  implicit def queryFloatGroupedToTE(q: Query[Group[Float]]): QueryValueExpressionNode[Float, TFloat] =
    new QueryValueExpressionNode[Float, TFloat](q.copy(false, Nil).ast, floatTEF.createOutMapper)
  implicit def queryOptionFloatGroupedToTE(
    q: Query[Group[Option[Float]]]
  ): QueryValueExpressionNode[Option[Float], TOptionFloat] =
    new QueryValueExpressionNode[Option[Float], TOptionFloat](q.copy(false, Nil).ast, optionFloatTEF.createOutMapper)
  implicit def queryFloatMeasuredToTE(q: Query[Measures[Float]]): QueryValueExpressionNode[Float, TFloat] =
    new QueryValueExpressionNode[Float, TFloat](q.copy(false, Nil).ast, floatTEF.createOutMapper)
  implicit def queryOptionFloatMeasuredToTE(
    q: Query[Measures[Option[Float]]]
  ): QueryValueExpressionNode[Option[Float], TOptionFloat] =
    new QueryValueExpressionNode[Option[Float], TOptionFloat](q.copy(false, Nil).ast, optionFloatTEF.createOutMapper)

  implicit def queryDoubleToTE(q: Query[Double]): QueryValueExpressionNode[Double, TDouble] =
    new QueryValueExpressionNode[Double, TDouble](q.copy(false, Nil).ast, doubleTEF.createOutMapper)
  implicit def queryOptionDoubleToTE(
    q: Query[Option[Double]]
  ): QueryValueExpressionNode[Option[Double], TOptionDouble] =
    new QueryValueExpressionNode[Option[Double], TOptionDouble](q.copy(false, Nil).ast, optionDoubleTEF.createOutMapper)
  implicit def queryDoubleGroupedToTE(q: Query[Group[Double]]): QueryValueExpressionNode[Double, TDouble] =
    new QueryValueExpressionNode[Double, TDouble](q.copy(false, Nil).ast, doubleTEF.createOutMapper)
  implicit def queryOptionDoubleGroupedToTE(
    q: Query[Group[Option[Double]]]
  ): QueryValueExpressionNode[Option[Double], TOptionDouble] =
    new QueryValueExpressionNode[Option[Double], TOptionDouble](q.copy(false, Nil).ast, optionDoubleTEF.createOutMapper)
  implicit def queryDoubleMeasuredToTE(q: Query[Measures[Double]]): QueryValueExpressionNode[Double, TDouble] =
    new QueryValueExpressionNode[Double, TDouble](q.copy(false, Nil).ast, doubleTEF.createOutMapper)
  implicit def queryOptionDoubleMeasuredToTE(
    q: Query[Measures[Option[Double]]]
  ): QueryValueExpressionNode[Option[Double], TOptionDouble] =
    new QueryValueExpressionNode[Option[Double], TOptionDouble](q.copy(false, Nil).ast, optionDoubleTEF.createOutMapper)

  implicit def queryBigDecimalToTE(q: Query[BigDecimal]): QueryValueExpressionNode[BigDecimal, TBigDecimal] =
    new QueryValueExpressionNode[BigDecimal, TBigDecimal](q.copy(false, Nil).ast, bigDecimalTEF.createOutMapper)
  implicit def queryOptionBigDecimalToTE(
    q: Query[Option[BigDecimal]]
  ): QueryValueExpressionNode[Option[BigDecimal], TOptionBigDecimal] =
    new QueryValueExpressionNode[Option[BigDecimal], TOptionBigDecimal](
      q.copy(false, Nil).ast,
      optionBigDecimalTEF.createOutMapper
    )
  implicit def queryBigDecimalGroupedToTE(
    q: Query[Group[BigDecimal]]
  ): QueryValueExpressionNode[BigDecimal, TBigDecimal] =
    new QueryValueExpressionNode[BigDecimal, TBigDecimal](q.copy(false, Nil).ast, bigDecimalTEF.createOutMapper)
  implicit def queryOptionBigDecimalGroupedToTE(
    q: Query[Group[Option[BigDecimal]]]
  ): QueryValueExpressionNode[Option[BigDecimal], TOptionBigDecimal] =
    new QueryValueExpressionNode[Option[BigDecimal], TOptionBigDecimal](
      q.copy(false, Nil).ast,
      optionBigDecimalTEF.createOutMapper
    )
  implicit def queryBigDecimalMeasuredToTE(
    q: Query[Measures[BigDecimal]]
  ): QueryValueExpressionNode[BigDecimal, TBigDecimal] =
    new QueryValueExpressionNode[BigDecimal, TBigDecimal](q.copy(false, Nil).ast, bigDecimalTEF.createOutMapper)
  implicit def queryOptionBigDecimalMeasuredToTE(
    q: Query[Measures[Option[BigDecimal]]]
  ): QueryValueExpressionNode[Option[BigDecimal], TOptionBigDecimal] =
    new QueryValueExpressionNode[Option[BigDecimal], TOptionBigDecimal](
      q.copy(false, Nil).ast,
      optionBigDecimalTEF.createOutMapper
    )

}
