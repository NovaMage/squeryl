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

import org.squeryl.internals.FieldMapper

@deprecated("the PrimitiveTypeMode companion object is deprecated, you should define a mix in the trait for your application. See : http://squeryl.org/0.9.6.html",
    "0.9.6")
object PrimitiveTypeMode extends PrimitiveTypeMode

private [squeryl] object InternalFieldMapper extends PrimitiveTypeMode

trait PrimitiveTypeMode extends QueryDsl with FieldMapper {


  // =========================== Non Numerical =========================== 
  implicit val stringTEF = PrimitiveTypeSupport.stringTEF
  implicit val optionStringTEF = PrimitiveTypeSupport.optionStringTEF
  implicit val dateTEF = PrimitiveTypeSupport.dateTEF
  implicit val optionDateTEF = PrimitiveTypeSupport.optionDateTEF
  implicit val sqlDateTEF = PrimitiveTypeSupport.sqlDateTEF
  implicit val optionSqlDateTEF = PrimitiveTypeSupport.optionSqlDateTEF
  implicit val localDateTEF = PrimitiveTypeSupport.localDateTEF
  implicit val optionLocalDateTEF = PrimitiveTypeSupport.optionLocalDateTEF
  implicit val localTimeTEF = PrimitiveTypeSupport.localTimeTEF
  implicit val optionLocalTimeTEF = PrimitiveTypeSupport.optionLocalTimeTEF
  implicit val timestampTEF = PrimitiveTypeSupport.timestampTEF
  implicit val optionTimestampTEF = PrimitiveTypeSupport.optionTimestampTEF
  implicit val localDateTimeTEF = PrimitiveTypeSupport.localDateTimeTEF
  implicit val optionLocalDateTimeTEF = PrimitiveTypeSupport.optionLocalDateTimeTEF
  implicit val offsetTimeTEF = PrimitiveTypeSupport.offsetTimeTEF
  implicit val optionOffsetTimeTEF = PrimitiveTypeSupport.optionOffsetTimeTEF
  implicit val instantTEF = PrimitiveTypeSupport.instantTEF
  implicit val optionInstantTEF = PrimitiveTypeSupport.optionInstantTEF
  implicit val offsetDateTimeTEF = PrimitiveTypeSupport.offsetDateTimeTEF
  implicit val optionOffsetDateTimeTEF = PrimitiveTypeSupport.optionOffsetDateTimeTEF
  implicit val doubleArrayTEF = PrimitiveTypeSupport.doubleArrayTEF
  implicit val intArrayTEF = PrimitiveTypeSupport.intArrayTEF
  implicit val longArrayTEF = PrimitiveTypeSupport.longArrayTEF
  implicit val stringArrayTEF = PrimitiveTypeSupport.stringArrayTEF

  // =========================== Numerical Integral =========================== 
  implicit val byteTEF = PrimitiveTypeSupport.byteTEF
  implicit val optionByteTEF = PrimitiveTypeSupport.optionByteTEF
  implicit val intTEF = PrimitiveTypeSupport.intTEF
  implicit val optionIntTEF = PrimitiveTypeSupport.optionIntTEF
  implicit val longTEF = PrimitiveTypeSupport.longTEF
  implicit val optionLongTEF = PrimitiveTypeSupport.optionLongTEF

  // =========================== Numerical Floating Point ===========================   
  implicit val floatTEF = PrimitiveTypeSupport.floatTEF
  implicit val optionFloatTEF = PrimitiveTypeSupport.optionFloatTEF
  implicit val doubleTEF = PrimitiveTypeSupport.doubleTEF
  implicit val optionDoubleTEF = PrimitiveTypeSupport.optionDoubleTEF
  implicit val bigDecimalTEF = PrimitiveTypeSupport.bigDecimalTEF
  implicit val optionBigDecimalTEF = PrimitiveTypeSupport.optionBigDecimalTEF


  implicit def stringToTE(s: String) = stringTEF.create(s)
  implicit def optionStringToTE(s: Option[String]) = optionStringTEF.create(s)

  implicit def dateToTE(s: Date) = dateTEF.create(s)
  implicit def optionDateToTE(s: Option[Date]) = optionDateTEF.create(s)

  implicit def localDateToTE(s: LocalDate) = localDateTEF.create(s)
  implicit def optionLocalDateToTE(s: Option[LocalDate]) = optionLocalDateTEF.create(s)

  implicit def localTimeToTE(s: LocalTime) = localTimeTEF.create(s)
  implicit def optionLocalTimeToTE(s: Option[LocalTime]) = optionLocalTimeTEF.create(s)

  implicit def timestampToTE(s: Timestamp) = timestampTEF.create(s)
  implicit def optionTimestampToTE(s: Option[Timestamp]) = optionTimestampTEF.create(s)

  implicit def localDateTimeToTE(s: LocalDateTime) = localDateTimeTEF.create(s)
  implicit def optionLocalDateTimeToTE(s: Option[LocalDateTime]) = optionLocalDateTimeTEF.create(s)

  implicit def offsetTimeToTE(s: OffsetTime) = offsetTimeTEF.create(s)
  implicit def optionOffsetTimeToTE(s: Option[OffsetTime]) = optionOffsetTimeTEF.create(s)

  implicit def instantToTE(s: Instant) = instantTEF.create(s)

  implicit def optionInstantToTE(s: Option[Instant]) = optionInstantTEF.create(s)

  implicit def offsetDateTimeToTE(s: OffsetDateTime) = offsetDateTimeTEF.create(s)
  implicit def optionOffsetDateTimeToTE(s: Option[OffsetDateTime]) = optionOffsetDateTimeTEF.create(s)

  implicit def zonedDateTimeToInstantTE(s: ZonedDateTime) = instantTEF.create(s.toInstant)
  implicit def optionZonedDateTimeToInstantTE(s: Some[ZonedDateTime]) = optionInstantTEF.create(s.map(_.toInstant))

  implicit def booleanToTE(s: Boolean) = PrimitiveTypeSupport.booleanTEF.create(s)
  implicit def optionBooleanToTE(s: Option[Boolean]) = PrimitiveTypeSupport.optionBooleanTEF.create(s)

  implicit def uuidToTE(s: UUID) = PrimitiveTypeSupport.uuidTEF.create(s)
  implicit def optionUUIDToTE(s: Option[UUID]) = PrimitiveTypeSupport.optionUUIDTEF.create(s)

  implicit def binaryToTE(s: Array[Byte]) = PrimitiveTypeSupport.binaryTEF.create(s)
  implicit def optionByteArrayToTE(s: Option[Array[Byte]]) = PrimitiveTypeSupport.optionByteArrayTEF.create(s)

  implicit def enumValueToTE[A >: Enumeration#Value <: Enumeration#Value](e: A): TypedExpression[A, TEnumValue[A]] =
    PrimitiveTypeSupport.enumValueTEF[A](e).create(e)

  implicit def optionEnumcValueToTE[A >: Enumeration#Value <: Enumeration#Value](e: Option[A]): TypedExpression[Option[A], TOptionEnumValue[A]] =
    PrimitiveTypeSupport.optionEnumValueTEF[A](e).create(e)

  implicit def byteToTE(f: Byte) = byteTEF.create(f)
  implicit def optionByteToTE(f: Option[Byte]) = optionByteTEF.create(f)

  implicit def intToTE(f: Int) = intTEF.create(f)
  implicit def optionIntToTE(f: Option[Int]) = optionIntTEF.create(f)

  implicit def longToTE(f: Long) = longTEF.create(f)
  implicit def optionLongToTE(f: Option[Long]) = optionLongTEF.create(f)

  implicit def floatToTE(f: Float) = floatTEF.create(f)
  implicit def optionFloatToTE(f: Option[Float]) = optionFloatTEF.create(f)

  implicit def doubleToTE(f: Double) = doubleTEF.create(f)
  implicit def optionDoubleToTE(f: Option[Double]) = optionDoubleTEF.create(f)

  implicit def bigDecimalToTE(f: BigDecimal) = bigDecimalTEF.create(f)
  implicit def optionBigDecimalToTE(f: Option[BigDecimal]) = optionBigDecimalTEF.create(f)

  implicit def doubleArrayToTE(f : Array[Double]) = doubleArrayTEF.create(f)
  implicit def intArrayToTE(f : Array[Int]) = intArrayTEF.create(f)
  implicit def longArrayToTE(f : Array[Long]) = longArrayTEF.create(f)
  implicit def stringArrayToTE(f: Array[String]) = stringArrayTEF.create(f)


  implicit def logicalBooleanToTE(l: LogicalBoolean) =
    PrimitiveTypeSupport.booleanTEF.convert(l)

  implicit def queryStringToTE(q: Query[String]) =
    new QueryValueExpressionNode[String, TString](q.copy(false, Nil).ast, stringTEF.createOutMapper)
  implicit def queryOptionStringToTE(q: Query[Option[String]]) =
    new QueryValueExpressionNode[Option[String], TOptionString](q.copy(false, Nil).ast, optionStringTEF.createOutMapper)
  implicit def queryStringGroupedToTE(q: Query[Group[String]]) =
    new QueryValueExpressionNode[String, TString](q.copy(false, Nil).ast, stringTEF.createOutMapper)
  implicit def queryOptionStringGroupedToTE(q: Query[Group[Option[String]]]) =
    new QueryValueExpressionNode[Option[String], TOptionString](q.copy(false, Nil).ast, optionStringTEF.createOutMapper)
  implicit def queryStringMeasuredToTE(q: Query[Measures[String]]) =
    new QueryValueExpressionNode[String, TString](q.copy(false, Nil).ast, stringTEF.createOutMapper)
  implicit def queryOptionStringMeasuredToTE(q: Query[Measures[Option[String]]]) =
    new QueryValueExpressionNode[Option[String], TOptionString](q.copy(false, Nil).ast, optionStringTEF.createOutMapper)

  implicit def queryDateToTE(q: Query[Date]) =
    new QueryValueExpressionNode[Date, TDate](q.copy(false, Nil).ast, dateTEF.createOutMapper)
  implicit def queryOptionDateToTE(q: Query[Option[Date]]) =
    new QueryValueExpressionNode[Option[Date], TOptionDate](q.copy(false, Nil).ast, optionDateTEF.createOutMapper)
  implicit def queryDateGroupedToTE(q: Query[Group[Date]]) =
    new QueryValueExpressionNode[Date, TDate](q.copy(false, Nil).ast, dateTEF.createOutMapper)
  implicit def queryOptionDateGroupedToTE(q: Query[Group[Option[Date]]]) =
    new QueryValueExpressionNode[Option[Date], TOptionDate](q.copy(false, Nil).ast, optionDateTEF.createOutMapper)
  implicit def queryDateMeasuredToTE(q: Query[Measures[Date]]) =
    new QueryValueExpressionNode[Date, TDate](q.copy(false, Nil).ast, dateTEF.createOutMapper)
  implicit def queryOptionDateMeasuredToTE(q: Query[Measures[Option[Date]]]) =
    new QueryValueExpressionNode[Option[Date], TOptionDate](q.copy(false, Nil).ast, optionDateTEF.createOutMapper)

  implicit def queryLocalDateToTE(q: Query[LocalDate]) =
    new QueryValueExpressionNode[LocalDate, TLocalDate](q.copy(false, Nil).ast, localDateTEF.createOutMapper)
  implicit def queryOptionLocalDateToTE(q: Query[Option[LocalDate]]) =
    new QueryValueExpressionNode[Option[LocalDate], TOptionLocalDate](q.copy(false, Nil).ast, optionLocalDateTEF.createOutMapper)
  implicit def queryLocalDateGroupedToTE(q: Query[Group[LocalDate]]) =
    new QueryValueExpressionNode[LocalDate, TLocalDate](q.copy(false, Nil).ast, localDateTEF.createOutMapper)
  implicit def queryOptionLocalDateGroupedToTE(q: Query[Group[Option[LocalDate]]]) =
    new QueryValueExpressionNode[Option[LocalDate], TOptionLocalDate](q.copy(false, Nil).ast, optionLocalDateTEF.createOutMapper)
  implicit def queryLocalDateMeasuredToTE(q: Query[Measures[LocalDate]]) =
    new QueryValueExpressionNode[LocalDate, TLocalDate](q.copy(false, Nil).ast, localDateTEF.createOutMapper)
  implicit def queryOptionLocalDateMeasuredToTE(q: Query[Measures[Option[LocalDate]]]) =
    new QueryValueExpressionNode[Option[LocalDate], TOptionLocalDate](q.copy(false, Nil).ast, optionLocalDateTEF.createOutMapper)

  implicit def queryLocalTimeToTE(q: Query[LocalTime]) =
    new QueryValueExpressionNode[LocalTime, TLocalTime](q.copy(false, Nil).ast, localTimeTEF.createOutMapper)
  implicit def queryOptionLocalTimeToTE(q: Query[Option[LocalTime]]) =
    new QueryValueExpressionNode[Option[LocalTime], TOptionLocalTime](q.copy(false, Nil).ast, optionLocalTimeTEF.createOutMapper)
  implicit def queryLocalTimeGroupedToTE(q: Query[Group[LocalTime]]) =
    new QueryValueExpressionNode[LocalTime, TLocalTime](q.copy(false, Nil).ast, localTimeTEF.createOutMapper)
  implicit def queryOptionLocalTimeGroupedToTE(q: Query[Group[Option[LocalTime]]]) =
    new QueryValueExpressionNode[Option[LocalTime], TOptionLocalTime](q.copy(false, Nil).ast, optionLocalTimeTEF.createOutMapper)
  implicit def queryLocalTimeMeasuredToTE(q: Query[Measures[LocalTime]]) =
    new QueryValueExpressionNode[LocalTime, TLocalTime](q.copy(false, Nil).ast, localTimeTEF.createOutMapper)
  implicit def queryOptionLocalTimeMeasuredToTE(q: Query[Measures[Option[LocalTime]]]) =
    new QueryValueExpressionNode[Option[LocalTime], TOptionLocalTime](q.copy(false, Nil).ast, optionLocalTimeTEF.createOutMapper)

  implicit def queryTimestampToTE(q: Query[Timestamp]) =
    new QueryValueExpressionNode[Timestamp, TTimestamp](q.copy(false, Nil).ast, timestampTEF.createOutMapper)
  implicit def queryOptionTimestampToTE(q: Query[Option[Timestamp]]) =
    new QueryValueExpressionNode[Option[Timestamp], TOptionTimestamp](q.copy(false, Nil).ast, optionTimestampTEF.createOutMapper)
  implicit def queryTimestampGroupedToTE(q: Query[Group[Timestamp]]) =
    new QueryValueExpressionNode[Timestamp, TTimestamp](q.copy(false, Nil).ast, timestampTEF.createOutMapper)
  implicit def queryOptionTimestampGroupedToTE(q: Query[Group[Option[Timestamp]]]) =
    new QueryValueExpressionNode[Option[Timestamp], TOptionTimestamp](q.copy(false, Nil).ast, optionTimestampTEF.createOutMapper)
  implicit def queryTimestampMeasuredToTE(q: Query[Measures[Timestamp]]) =
    new QueryValueExpressionNode[Timestamp, TTimestamp](q.copy(false, Nil).ast, timestampTEF.createOutMapper)
  implicit def queryOptionTimestampMeasuredToTE(q: Query[Measures[Option[Timestamp]]]) =
    new QueryValueExpressionNode[Option[Timestamp], TOptionTimestamp](q.copy(false, Nil).ast, optionTimestampTEF.createOutMapper)

  implicit def queryLocalDateTimeToTE(q: Query[LocalDateTime]) =
    new QueryValueExpressionNode[LocalDateTime, TLocalDateTime](q.copy(false, Nil).ast, localDateTimeTEF.createOutMapper)
  implicit def queryOptionLocalDateTimeToTE(q: Query[Option[LocalDateTime]]) =
    new QueryValueExpressionNode[Option[LocalDateTime], TOptionLocalDateTime](q.copy(false, Nil).ast, optionLocalDateTimeTEF.createOutMapper)
  implicit def queryLocalDateTimeGroupedToTE(q: Query[Group[LocalDateTime]]) =
    new QueryValueExpressionNode[LocalDateTime, TLocalDateTime](q.copy(false, Nil).ast, localDateTimeTEF.createOutMapper)
  implicit def queryOptionLocalDateTimeGroupedToTE(q: Query[Group[Option[LocalDateTime]]]) =
    new QueryValueExpressionNode[Option[LocalDateTime], TOptionLocalDateTime](q.copy(false, Nil).ast, optionLocalDateTimeTEF.createOutMapper)
  implicit def queryLocalDateTimeMeasuredToTE(q: Query[Measures[LocalDateTime]]) =
    new QueryValueExpressionNode[LocalDateTime, TLocalDateTime](q.copy(false, Nil).ast, localDateTimeTEF.createOutMapper)
  implicit def queryOptionLocalDateTimeMeasuredToTE(q: Query[Measures[Option[LocalDateTime]]]) =
    new QueryValueExpressionNode[Option[LocalDateTime], TOptionLocalDateTime](q.copy(false, Nil).ast, optionLocalDateTimeTEF.createOutMapper)

  implicit def queryOffsetTimeToTE(q: Query[OffsetTime]) =
    new QueryValueExpressionNode[OffsetTime, TOffsetTime](q.copy(false, Nil).ast, offsetTimeTEF.createOutMapper)
  implicit def queryOptionOffsetTimeToTE(q: Query[Option[OffsetTime]]) =
    new QueryValueExpressionNode[Option[OffsetTime], TOptionOffsetTime](q.copy(false, Nil).ast, optionOffsetTimeTEF.createOutMapper)
  implicit def queryOffsetTimeGroupedToTE(q: Query[Group[OffsetTime]]) =
    new QueryValueExpressionNode[OffsetTime, TOffsetTime](q.copy(false, Nil).ast, offsetTimeTEF.createOutMapper)
  implicit def queryOptionOffsetTimeGroupedToTE(q: Query[Group[Option[OffsetTime]]]) =
    new QueryValueExpressionNode[Option[OffsetTime], TOptionOffsetTime](q.copy(false, Nil).ast, optionOffsetTimeTEF.createOutMapper)
  implicit def queryOffsetTimeMeasuredToTE(q: Query[Measures[OffsetTime]]) =
    new QueryValueExpressionNode[OffsetTime, TOffsetTime](q.copy(false, Nil).ast, offsetTimeTEF.createOutMapper)
  implicit def queryOptionOffsetTimeMeasuredToTE(q: Query[Measures[Option[OffsetTime]]]) =
    new QueryValueExpressionNode[Option[OffsetTime], TOptionOffsetTime](q.copy(false, Nil).ast, optionOffsetTimeTEF.createOutMapper)

  implicit def queryInstantToTE(q: Query[Instant]) =
    new QueryValueExpressionNode[Instant, TInstant](q.copy(false, Nil).ast, instantTEF.createOutMapper)

  implicit def queryOptionInstantToTE(q: Query[Option[Instant]]) =
    new QueryValueExpressionNode[Option[Instant], TOptionInstant](q.copy(false, Nil).ast, optionInstantTEF.createOutMapper)

  implicit def queryInstantGroupedToTE(q: Query[Group[Instant]]) =
    new QueryValueExpressionNode[Instant, TInstant](q.copy(false, Nil).ast, instantTEF.createOutMapper)

  implicit def queryOptionInstantGroupedToTE(q: Query[Group[Option[Instant]]]) =
    new QueryValueExpressionNode[Option[Instant], TOptionInstant](q.copy(false, Nil).ast, optionInstantTEF.createOutMapper)

  implicit def queryInstantMeasuredToTE(q: Query[Measures[Instant]]) =
    new QueryValueExpressionNode[Instant, TInstant](q.copy(false, Nil).ast, instantTEF.createOutMapper)

  implicit def queryOptionInstantMeasuredToTE(q: Query[Measures[Option[Instant]]]) =
    new QueryValueExpressionNode[Option[Instant], TOptionInstant](q.copy(false, Nil).ast, optionInstantTEF.createOutMapper)

  implicit def queryOffsetDateTimeToTE(q: Query[OffsetDateTime]) =
    new QueryValueExpressionNode[OffsetDateTime, TOffsetDateTime](q.copy(false, Nil).ast, offsetDateTimeTEF.createOutMapper)
  implicit def queryOptionOffsetDateTimeToTE(q: Query[Option[OffsetDateTime]]) =
    new QueryValueExpressionNode[Option[OffsetDateTime], TOptionOffsetDateTime](q.copy(false, Nil).ast, optionOffsetDateTimeTEF.createOutMapper)
  implicit def queryOffsetDateTimeGroupedToTE(q: Query[Group[OffsetDateTime]]) =
    new QueryValueExpressionNode[OffsetDateTime, TOffsetDateTime](q.copy(false, Nil).ast, offsetDateTimeTEF.createOutMapper)
  implicit def queryOptionOffsetDateTimeGroupedToTE(q: Query[Group[Option[OffsetDateTime]]]) =
    new QueryValueExpressionNode[Option[OffsetDateTime], TOptionOffsetDateTime](q.copy(false, Nil).ast, optionOffsetDateTimeTEF.createOutMapper)
  implicit def queryOffsetDateTimeMeasuredToTE(q: Query[Measures[OffsetDateTime]]) =
    new QueryValueExpressionNode[OffsetDateTime, TOffsetDateTime](q.copy(false, Nil).ast, offsetDateTimeTEF.createOutMapper)
  implicit def queryOptionOffsetDateTimeMeasuredToTE(q: Query[Measures[Option[OffsetDateTime]]]) =
    new QueryValueExpressionNode[Option[OffsetDateTime], TOptionOffsetDateTime](q.copy(false, Nil).ast, optionOffsetDateTimeTEF.createOutMapper)

  implicit def queryBooleanToTE(q: Query[Boolean]) =
    new QueryValueExpressionNode[Boolean, TBoolean](q.copy(false, Nil).ast, PrimitiveTypeSupport.booleanTEF.createOutMapper)
  implicit def queryOptionBooleanToTE(q: Query[Option[Boolean]]) =
    new QueryValueExpressionNode[Option[Boolean], TOptionBoolean](q.copy(false, Nil).ast, PrimitiveTypeSupport.optionBooleanTEF.createOutMapper)

  implicit def queryUUIDToTE(q: Query[UUID]) =
    new QueryValueExpressionNode[UUID, TUUID](q.copy(false, Nil).ast, PrimitiveTypeSupport.uuidTEF.createOutMapper)
  implicit def queryOptionUUIDToTE(q: Query[Option[UUID]]) =
    new QueryValueExpressionNode[Option[UUID], TOptionUUID](q.copy(false, Nil).ast, PrimitiveTypeSupport.optionUUIDTEF.createOutMapper)

  implicit def queryByteArrayToTE(q: Query[Array[Byte]]) =
    new QueryValueExpressionNode[Array[Byte], TByteArray](q.copy(false, Nil).ast, PrimitiveTypeSupport.binaryTEF.createOutMapper)
  implicit def queryOptionByteArrayToTE(q: Query[Option[Array[Byte]]]) =
    new QueryValueExpressionNode[Option[Array[Byte]], TOptionByteArray](q.copy(false, Nil).ast, PrimitiveTypeSupport.optionByteArrayTEF.createOutMapper)

  implicit def queryByteToTE(q: Query[Byte]) =
    new QueryValueExpressionNode[Byte, TByte](q.copy(false, Nil).ast, byteTEF.createOutMapper)
  implicit def queryOptionByteToTE(q: Query[Option[Byte]]) =
    new QueryValueExpressionNode[Option[Byte], TOptionByte](q.copy(false, Nil).ast, optionByteTEF.createOutMapper)
  implicit def queryByteGroupedToTE(q: Query[Group[Byte]]) =
    new QueryValueExpressionNode[Byte, TByte](q.copy(false, Nil).ast, byteTEF.createOutMapper)
  implicit def queryOptionByteGroupedToTE(q: Query[Group[Option[Byte]]]) =
    new QueryValueExpressionNode[Option[Byte], TOptionByte](q.copy(false, Nil).ast, optionByteTEF.createOutMapper)
  implicit def queryByteMeasuredToTE(q: Query[Measures[Byte]]) =
    new QueryValueExpressionNode[Byte, TByte](q.copy(false, Nil).ast, byteTEF.createOutMapper)
  implicit def queryOptionByteMeasuredToTE(q: Query[Measures[Option[Byte]]]) =
    new QueryValueExpressionNode[Option[Byte], TOptionByte](q.copy(false, Nil).ast, optionByteTEF.createOutMapper)

  implicit def queryIntToTE(q: Query[Int]) =
    new QueryValueExpressionNode[Int, TInt](q.copy(false, Nil).ast, intTEF.createOutMapper)
  implicit def queryOptionIntToTE(q: Query[Option[Int]]) =
    new QueryValueExpressionNode[Option[Int], TOptionInt](q.copy(false, Nil).ast, optionIntTEF.createOutMapper)
  implicit def queryIntGroupedToTE(q: Query[Group[Int]]) =
    new QueryValueExpressionNode[Int, TInt](q.copy(false, Nil).ast, intTEF.createOutMapper)
  implicit def queryOptionIntGroupedToTE(q: Query[Group[Option[Int]]]) =
    new QueryValueExpressionNode[Option[Int], TOptionInt](q.copy(false, Nil).ast, optionIntTEF.createOutMapper)
  implicit def queryIntMeasuredToTE(q: Query[Measures[Int]]) =
    new QueryValueExpressionNode[Int, TInt](q.copy(false, Nil).ast, intTEF.createOutMapper)
  implicit def queryOptionIntMeasuredToTE(q: Query[Measures[Option[Int]]]) =
    new QueryValueExpressionNode[Option[Int], TOptionInt](q.copy(false, Nil).ast, optionIntTEF.createOutMapper)

  implicit def queryLongToTE(q: Query[Long]) =
    new QueryValueExpressionNode[Long, TLong](q.copy(false, Nil).ast, longTEF.createOutMapper)
  implicit def queryOptionLongToTE(q: Query[Option[Long]]) =
    new QueryValueExpressionNode[Option[Long], TOptionLong](q.copy(false, Nil).ast, optionLongTEF.createOutMapper)
  implicit def queryLongGroupedToTE(q: Query[Group[Long]]) =
    new QueryValueExpressionNode[Long, TLong](q.copy(false, Nil).ast, longTEF.createOutMapper)
  implicit def queryOptionLongGroupedToTE(q: Query[Group[Option[Long]]]) =
    new QueryValueExpressionNode[Option[Long], TOptionLong](q.copy(false, Nil).ast, optionLongTEF.createOutMapper)
  implicit def queryLongMeasuredToTE(q: Query[Measures[Long]]) =
    new QueryValueExpressionNode[Long, TLong](q.copy(false, Nil).ast, longTEF.createOutMapper)
  implicit def queryOptionLongMeasuredToTE(q: Query[Measures[Option[Long]]]) =
    new QueryValueExpressionNode[Option[Long], TOptionLong](q.copy(false, Nil).ast, optionLongTEF.createOutMapper)

  implicit def queryFloatToTE(q: Query[Float]) =
    new QueryValueExpressionNode[Float, TFloat](q.copy(false, Nil).ast, floatTEF.createOutMapper)
  implicit def queryOptionFloatToTE(q: Query[Option[Float]]) =
    new QueryValueExpressionNode[Option[Float], TOptionFloat](q.copy(false, Nil).ast, optionFloatTEF.createOutMapper)
  implicit def queryFloatGroupedToTE(q: Query[Group[Float]]) =
    new QueryValueExpressionNode[Float, TFloat](q.copy(false, Nil).ast, floatTEF.createOutMapper)
  implicit def queryOptionFloatGroupedToTE(q: Query[Group[Option[Float]]]) =
    new QueryValueExpressionNode[Option[Float], TOptionFloat](q.copy(false, Nil).ast, optionFloatTEF.createOutMapper)
  implicit def queryFloatMeasuredToTE(q: Query[Measures[Float]]) =
    new QueryValueExpressionNode[Float, TFloat](q.copy(false, Nil).ast, floatTEF.createOutMapper)
  implicit def queryOptionFloatMeasuredToTE(q: Query[Measures[Option[Float]]]) =
    new QueryValueExpressionNode[Option[Float], TOptionFloat](q.copy(false, Nil).ast, optionFloatTEF.createOutMapper)

  implicit def queryDoubleToTE(q: Query[Double]) =
    new QueryValueExpressionNode[Double, TDouble](q.copy(false, Nil).ast, doubleTEF.createOutMapper)
  implicit def queryOptionDoubleToTE(q: Query[Option[Double]]) =
    new QueryValueExpressionNode[Option[Double], TOptionDouble](q.copy(false, Nil).ast, optionDoubleTEF.createOutMapper)
  implicit def queryDoubleGroupedToTE(q: Query[Group[Double]]) =
    new QueryValueExpressionNode[Double, TDouble](q.copy(false, Nil).ast, doubleTEF.createOutMapper)
  implicit def queryOptionDoubleGroupedToTE(q: Query[Group[Option[Double]]]) =
    new QueryValueExpressionNode[Option[Double], TOptionDouble](q.copy(false, Nil).ast, optionDoubleTEF.createOutMapper)
  implicit def queryDoubleMeasuredToTE(q: Query[Measures[Double]]) =
    new QueryValueExpressionNode[Double, TDouble](q.copy(false, Nil).ast, doubleTEF.createOutMapper)
  implicit def queryOptionDoubleMeasuredToTE(q: Query[Measures[Option[Double]]]) =
    new QueryValueExpressionNode[Option[Double], TOptionDouble](q.copy(false, Nil).ast, optionDoubleTEF.createOutMapper)

  implicit def queryBigDecimalToTE(q: Query[BigDecimal]) =
    new QueryValueExpressionNode[BigDecimal, TBigDecimal](q.copy(false, Nil).ast, bigDecimalTEF.createOutMapper)
  implicit def queryOptionBigDecimalToTE(q: Query[Option[BigDecimal]]) =
    new QueryValueExpressionNode[Option[BigDecimal], TOptionBigDecimal](q.copy(false, Nil).ast, optionBigDecimalTEF.createOutMapper)
  implicit def queryBigDecimalGroupedToTE(q: Query[Group[BigDecimal]]) =
    new QueryValueExpressionNode[BigDecimal, TBigDecimal](q.copy(false, Nil).ast, bigDecimalTEF.createOutMapper)
  implicit def queryOptionBigDecimalGroupedToTE(q: Query[Group[Option[BigDecimal]]]) =
    new QueryValueExpressionNode[Option[BigDecimal], TOptionBigDecimal](q.copy(false, Nil).ast, optionBigDecimalTEF.createOutMapper)
  implicit def queryBigDecimalMeasuredToTE(q: Query[Measures[BigDecimal]]) =
    new QueryValueExpressionNode[BigDecimal, TBigDecimal](q.copy(false, Nil).ast, bigDecimalTEF.createOutMapper)
  implicit def queryOptionBigDecimalMeasuredToTE(q: Query[Measures[Option[BigDecimal]]]) =
    new QueryValueExpressionNode[Option[BigDecimal], TOptionBigDecimal](q.copy(false, Nil).ast, optionBigDecimalTEF.createOutMapper)

}
