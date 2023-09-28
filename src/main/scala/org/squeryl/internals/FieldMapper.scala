/** *****************************************************************************
 * Copyright 2010 Maxime Lévesque
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * **************************************************************************** */

package org.squeryl.internals

import java.sql.{ResultSet, Timestamp}
import java.time._
import java.util.{Date, UUID}
import org.squeryl.TargetsValuesSupertype
import org.squeryl.dsl.{ArrayJdbcMapper, _}

import java.sql
import scala.collection.mutable.HashMap

trait FieldMapper {
  outer =>

  private[this] val registry = new HashMap[Class[_], FieldAttributesBasedOnType[_]]

  implicit def thisFieldMapper: FieldMapper = this

  /**
   * Extending classes will expose members of PrimitiveTypeSupport as implicit, to enable
   * support of primitive types, or will expose theit own non jdbc native types.
   */

  protected object PrimitiveTypeSupport {
    // =========================== Non Numerical ===========================

    val stringTEF: TypedExpressionFactory[String, TString] with PrimitiveJdbcMapper[String] =
      new TypedExpressionFactory[String, TString] with PrimitiveJdbcMapper[String] {
        val sample: String = "": String
        val defaultColumnLength = 128

        def extractNativeJdbcValue(rs: ResultSet, i: Int): String = rs.getString(i)
      }

    val optionStringTEF: TypedExpressionFactory[Option[String], TOptionString]
      with DeOptionizer[String, String, TString, Option[String], TOptionString] =
      new TypedExpressionFactory[Option[String], TOptionString]
        with DeOptionizer[String, String, TString, Option[String], TOptionString] {
        val deOptionizer: TypedExpressionFactory[String, TString] with JdbcMapper[String, String] = stringTEF
      }

    val dateTEF: TypedExpressionFactory[Date, TDate] with PrimitiveJdbcMapper[Date] =
      new TypedExpressionFactory[Date, TDate] with PrimitiveJdbcMapper[Date] {
        val sample = new Date
        val defaultColumnLength: Int = -1

        def extractNativeJdbcValue(rs: ResultSet, i: Int): Date = rs.getDate(i)
      }

    val sqlDateTEF: TypedExpressionFactory[sql.Date, TDate] with PrimitiveJdbcMapper[sql.Date] =
      new TypedExpressionFactory[java.sql.Date, TDate] with PrimitiveJdbcMapper[java.sql.Date] {
        val sample = new java.sql.Date(0L)
        val defaultColumnLength: Int = -1

        def extractNativeJdbcValue(rs: ResultSet, i: Int): sql.Date = rs.getDate(i)
      }

    val localDateTEF: TypedExpressionFactory[LocalDate, TLocalDate] with PrimitiveJdbcMapper[LocalDate] =
      new TypedExpressionFactory[LocalDate, TLocalDate] with PrimitiveJdbcMapper[LocalDate] {
        val sample: LocalDate = LocalDate.now()
        val defaultColumnLength: Int = -1

        def extractNativeJdbcValue(rs: ResultSet, i: Int): LocalDate = rs.getObject(i, classOf[LocalDate])
      }

    val localTimeTEF: TypedExpressionFactory[LocalTime, TLocalTime] with PrimitiveJdbcMapper[LocalTime] =
      new TypedExpressionFactory[LocalTime, TLocalTime] with PrimitiveJdbcMapper[LocalTime] {
        val sample: LocalTime = LocalTime.now()
        val defaultColumnLength: Int = -1

        def extractNativeJdbcValue(rs: ResultSet, i: Int): LocalTime = rs.getObject(i, classOf[LocalTime])
      }

    val optionDateTEF: TypedExpressionFactory[Option[Date], TOptionDate]
      with DeOptionizer[Date, Date, TDate, Option[Date], TOptionDate] =
      new TypedExpressionFactory[Option[Date], TOptionDate]
        with DeOptionizer[Date, Date, TDate, Option[Date], TOptionDate] {
        val deOptionizer: TypedExpressionFactory[Date, TDate] with JdbcMapper[Date, Date] = dateTEF
      }

    val optionSqlDateTEF: TypedExpressionFactory[Option[sql.Date], TOptionDate]
      with DeOptionizer[sql.Date, sql.Date, TDate, Option[sql.Date], TOptionDate] =
      new TypedExpressionFactory[Option[java.sql.Date], TOptionDate]
        with DeOptionizer[java.sql.Date, java.sql.Date, TDate, Option[java.sql.Date], TOptionDate] {
        val deOptionizer: TypedExpressionFactory[sql.Date, TDate] with JdbcMapper[sql.Date, sql.Date] = sqlDateTEF
      }

    val optionLocalDateTEF: TypedExpressionFactory[Option[LocalDate], TOptionLocalDate]
      with DeOptionizer[LocalDate, LocalDate, TLocalDate, Option[LocalDate], TOptionLocalDate] =
      new TypedExpressionFactory[Option[LocalDate], TOptionLocalDate]
        with DeOptionizer[LocalDate, LocalDate, TLocalDate, Option[LocalDate], TOptionLocalDate] {
        override val deOptionizer: TypedExpressionFactory[LocalDate, TLocalDate] with JdbcMapper[LocalDate, LocalDate] =
          localDateTEF
      }

    val optionLocalTimeTEF: TypedExpressionFactory[Option[LocalTime], TOptionLocalTime]
      with DeOptionizer[LocalTime, LocalTime, TLocalTime, Option[LocalTime], TOptionLocalTime] =
      new TypedExpressionFactory[Option[LocalTime], TOptionLocalTime]
        with DeOptionizer[LocalTime, LocalTime, TLocalTime, Option[LocalTime], TOptionLocalTime] {
        override val deOptionizer: TypedExpressionFactory[LocalTime, TLocalTime] with JdbcMapper[LocalTime, LocalTime] =
          localTimeTEF
      }

    val timestampTEF: TypedExpressionFactory[Timestamp, TTimestamp] with PrimitiveJdbcMapper[Timestamp] =
      new TypedExpressionFactory[Timestamp, TTimestamp] with PrimitiveJdbcMapper[Timestamp] {
        val sample = new Timestamp(0)
        val defaultColumnLength: Int = -1

        def extractNativeJdbcValue(rs: ResultSet, i: Int): Timestamp = rs.getTimestamp(i)
      }

    val localDateTimeTEF
      : TypedExpressionFactory[LocalDateTime, TLocalDateTime] with PrimitiveJdbcMapper[LocalDateTime] =
      new TypedExpressionFactory[LocalDateTime, TLocalDateTime] with PrimitiveJdbcMapper[LocalDateTime] {
        val sample: LocalDateTime = LocalDateTime.now()
        val defaultColumnLength: Int = -1

        def extractNativeJdbcValue(rs: ResultSet, i: Int): LocalDateTime = rs.getObject(i, classOf[LocalDateTime])
      }

    val offsetTimeTEF: TypedExpressionFactory[OffsetTime, TOffsetTime] with PrimitiveJdbcMapper[OffsetTime] =
      new TypedExpressionFactory[OffsetTime, TOffsetTime] with PrimitiveJdbcMapper[OffsetTime] {
        val sample: OffsetTime = OffsetTime.now()
        val defaultColumnLength: Int = -1

        def extractNativeJdbcValue(rs: ResultSet, i: Int): OffsetTime = rs.getObject(i, classOf[OffsetTime])
      }

    val offsetDateTimeTEF
      : TypedExpressionFactory[OffsetDateTime, TOffsetDateTime] with PrimitiveJdbcMapper[OffsetDateTime] =
      new TypedExpressionFactory[OffsetDateTime, TOffsetDateTime] with PrimitiveJdbcMapper[OffsetDateTime] {
        val sample: OffsetDateTime = OffsetDateTime.now()
        val defaultColumnLength: Int = -1

        def extractNativeJdbcValue(rs: ResultSet, i: Int): OffsetDateTime = rs.getObject(i, classOf[OffsetDateTime])
      }

    val instantTEF: NonPrimitiveJdbcMapper[OffsetDateTime, Instant, TInstant] =
      new NonPrimitiveJdbcMapper[OffsetDateTime, Instant, TInstant](offsetDateTimeTEF, thisFieldMapper) {
        override def convertFromJdbc(v: OffsetDateTime): Instant = if (v == null) null else v.toInstant

        override def convertToJdbc(v: Instant): OffsetDateTime = if (v == null) null else v.atOffset(ZoneOffset.UTC)
      }

    val optionTimestampTEF: TypedExpressionFactory[Option[Timestamp], TOptionTimestamp]
      with DeOptionizer[Timestamp, Timestamp, TTimestamp, Option[Timestamp], TOptionTimestamp] =
      new TypedExpressionFactory[Option[Timestamp], TOptionTimestamp]
        with DeOptionizer[Timestamp, Timestamp, TTimestamp, Option[Timestamp], TOptionTimestamp] {
        val deOptionizer: TypedExpressionFactory[Timestamp, TTimestamp] with JdbcMapper[Timestamp, Timestamp] =
          timestampTEF
      }

    val optionLocalDateTimeTEF: TypedExpressionFactory[Option[LocalDateTime], TOptionLocalDateTime]
      with DeOptionizer[LocalDateTime, LocalDateTime, TLocalDateTime, Option[LocalDateTime], TOptionLocalDateTime] =
      new TypedExpressionFactory[Option[LocalDateTime], TOptionLocalDateTime]
        with DeOptionizer[LocalDateTime, LocalDateTime, TLocalDateTime, Option[LocalDateTime], TOptionLocalDateTime] {
        val deOptionizer
          : TypedExpressionFactory[LocalDateTime, TLocalDateTime] with JdbcMapper[LocalDateTime, LocalDateTime] =
          localDateTimeTEF
      }

    val optionOffsetTimeTEF: TypedExpressionFactory[Option[OffsetTime], TOptionOffsetTime]
      with DeOptionizer[OffsetTime, OffsetTime, TOffsetTime, Option[OffsetTime], TOptionOffsetTime] =
      new TypedExpressionFactory[Option[OffsetTime], TOptionOffsetTime]
        with DeOptionizer[OffsetTime, OffsetTime, TOffsetTime, Option[OffsetTime], TOptionOffsetTime] {
        val deOptionizer: TypedExpressionFactory[OffsetTime, TOffsetTime] with JdbcMapper[OffsetTime, OffsetTime] =
          offsetTimeTEF
      }

    val optionOffsetDateTimeTEF: TypedExpressionFactory[Option[OffsetDateTime], TOptionOffsetDateTime]
      with DeOptionizer[OffsetDateTime, OffsetDateTime, TOffsetDateTime, Option[
        OffsetDateTime
      ], TOptionOffsetDateTime] = new TypedExpressionFactory[Option[OffsetDateTime], TOptionOffsetDateTime]
      with DeOptionizer[
        OffsetDateTime,
        OffsetDateTime,
        TOffsetDateTime,
        Option[OffsetDateTime],
        TOptionOffsetDateTime
      ] {
      val deOptionizer
        : TypedExpressionFactory[OffsetDateTime, TOffsetDateTime] with JdbcMapper[OffsetDateTime, OffsetDateTime] =
        offsetDateTimeTEF
    }

    val optionInstantTEF: TypedExpressionFactory[Option[Instant], TOptionInstant]
      with DeOptionizer[OffsetDateTime, Instant, TInstant, Option[Instant], TOptionInstant] =
      new TypedExpressionFactory[Option[Instant], TOptionInstant]
        with DeOptionizer[OffsetDateTime, Instant, TInstant, Option[Instant], TOptionInstant] {
        val deOptionizer: TypedExpressionFactory[Instant, TInstant] with JdbcMapper[OffsetDateTime, Instant] =
          instantTEF
      }

    val booleanTEF: TypedExpressionFactory[Boolean, TBoolean] with PrimitiveJdbcMapper[Boolean] =
      new TypedExpressionFactory[Boolean, TBoolean] with PrimitiveJdbcMapper[Boolean] {
        val sample = true
        val defaultColumnLength = 1

        def extractNativeJdbcValue(rs: ResultSet, i: Int): Boolean = rs.getBoolean(i)
      }

    val optionBooleanTEF: TypedExpressionFactory[Option[Boolean], TOptionBoolean]
      with DeOptionizer[Boolean, Boolean, TBoolean, Option[Boolean], TOptionBoolean] =
      new TypedExpressionFactory[Option[Boolean], TOptionBoolean]
        with DeOptionizer[Boolean, Boolean, TBoolean, Option[Boolean], TOptionBoolean] {
        val deOptionizer: TypedExpressionFactory[Boolean, TBoolean] with JdbcMapper[Boolean, Boolean] = booleanTEF
      }

    val uuidTEF: TypedExpressionFactory[UUID, TUUID] with PrimitiveJdbcMapper[UUID] =
      new TypedExpressionFactory[UUID, TUUID] with PrimitiveJdbcMapper[UUID] {
        val sample: UUID = java.util.UUID.fromString("00000000-0000-0000-0000-000000000000")
        val defaultColumnLength = 36

        def extractNativeJdbcValue(rs: ResultSet, i: Int): UUID = {
          val v = rs.getObject(i)
          v match {
            case u: UUID => u
            case s: String => UUID.fromString(s)
            case _ => sample
          }
        }
      }

    val optionUUIDTEF: TypedExpressionFactory[Option[UUID], TOptionUUID]
      with DeOptionizer[UUID, UUID, TUUID, Option[UUID], TOptionUUID] =
      new TypedExpressionFactory[Option[UUID], TOptionUUID]
        with DeOptionizer[UUID, UUID, TUUID, Option[UUID], TOptionUUID] {
        val deOptionizer: TypedExpressionFactory[UUID, TUUID] with JdbcMapper[UUID, UUID] = uuidTEF
      }

    val binaryTEF: TypedExpressionFactory[Array[Byte], TByteArray] with PrimitiveJdbcMapper[Array[Byte]] =
      new TypedExpressionFactory[Array[Byte], TByteArray] with PrimitiveJdbcMapper[Array[Byte]] {
        val sample: Array[Byte] = Array(0: Byte)
        val defaultColumnLength = 255

        def extractNativeJdbcValue(rs: ResultSet, i: Int): Array[Byte] = rs.getBytes(i)
      }

    val optionByteArrayTEF: TypedExpressionFactory[Option[Array[Byte]], TOptionByteArray]
      with DeOptionizer[Array[Byte], Array[Byte], TByteArray, Option[Array[Byte]], TOptionByteArray] =
      new TypedExpressionFactory[Option[Array[Byte]], TOptionByteArray]
        with DeOptionizer[Array[Byte], Array[Byte], TByteArray, Option[Array[Byte]], TOptionByteArray] {
        val deOptionizer: TypedExpressionFactory[Array[Byte], TByteArray] with JdbcMapper[Array[Byte], Array[Byte]] =
          binaryTEF
      }

    val intArrayTEF: ArrayTEF[Int, TIntArray] = new ArrayTEF[Int, TIntArray] {
      val sample: Array[Int] = Array(0)

      def toWrappedJDBCType(element: Int): java.lang.Object = java.lang.Integer.valueOf(element)

      def fromWrappedJDBCType(elements: Array[java.lang.Object]): Array[Int] =
        elements.map(i => i.asInstanceOf[java.lang.Integer].toInt)
    }

    val longArrayTEF: ArrayTEF[Long, TLongArray] = new ArrayTEF[Long, TLongArray] {
      val sample: Array[Long] = Array(0L)

      def toWrappedJDBCType(element: Long): java.lang.Object = java.lang.Long.valueOf(element)

      def fromWrappedJDBCType(elements: Array[java.lang.Object]): Array[Long] =
        elements.map(i => i.asInstanceOf[java.lang.Long].toLong)
    }

    val doubleArrayTEF: ArrayTEF[Double, TDoubleArray] = new ArrayTEF[Double, TDoubleArray] {
      val sample: Array[Double] = Array(0.0)

      def toWrappedJDBCType(element: Double): java.lang.Object = java.lang.Double.valueOf(element)

      def fromWrappedJDBCType(elements: Array[java.lang.Object]): Array[Double] =
        elements.map(i => i.asInstanceOf[java.lang.Double].toDouble)
    }

    val stringArrayTEF: ArrayTEF[String, TStringArray] = new ArrayTEF[String, TStringArray] {
      val sample: Array[String] = Array("")

      def toWrappedJDBCType(element: String): java.lang.Object = new java.lang.String(element)

      def fromWrappedJDBCType(elements: Array[java.lang.Object]): Array[String] =
        elements.map(i => i.asInstanceOf[java.lang.String].toString)
    }

    // FIXME: The type soup on this was beyond my patience for now...I think we'll need an ArrayDeOptionizer
    // val optionIntArrayTEF = new TypedExpressionFactory[Option[Array[Int]],TOptionIntArray] with DeOptionizer[Array[Int], Array[Int], TIntArray, Option[Array[Int]], TOptionIntArray] {
    // val deOptionizer = intArrayTEF
    // }

    def enumValueTEF[A >: Enumeration#Value <: Enumeration#Value](
      ev: Enumeration#Value
    ): JdbcMapper[Int, A] with TypedExpressionFactory[A, TEnumValue[A]] =
      new JdbcMapper[Int, A] with TypedExpressionFactory[A, TEnumValue[A]] {

        val enu: Enumeration = Utils.enumerationForValue(ev)

        def extractNativeJdbcValue(rs: ResultSet, i: Int): Int = rs.getInt(i)

        def defaultColumnLength: Int = intTEF.defaultColumnLength

        def sample: A = ev

        def convertToJdbc(v: A): Int = v.id

        def convertFromJdbc(v: Int): A = {
          enu.values
            .find(_.id == v)
            .getOrElse(
              DummyEnum.DummyEnumerationValue
            ) // JDBC has no concept of null value for primitive types (ex. Int)
          // at this level, we mimic this JDBC flaw (the Option / None based on jdbc.wasNull will get sorted out by optionEnumValueTEF)
        }
      }

    object DummyEnum extends Enumeration {
      type DummyEnum = Value
      val DummyEnumerationValue = Value(-1, "DummyEnumerationValue")
    }

    def optionEnumValueTEF[A >: Enumeration#Value <: Enumeration#Value](
      ev: Option[Enumeration#Value]
    ): TypedExpressionFactory[Option[A], TOptionEnumValue[A]]
      with DeOptionizer[Int, A, TEnumValue[A], Option[A], TOptionEnumValue[A]] =
      new TypedExpressionFactory[Option[A], TOptionEnumValue[A]]
        with DeOptionizer[Int, A, TEnumValue[A], Option[A], TOptionEnumValue[A]] {
        val deOptionizer: TypedExpressionFactory[A, TEnumValue[A]] with JdbcMapper[Int, A] = {
          val e = ev.getOrElse(PrimitiveTypeSupport.DummyEnum.DummyEnumerationValue)
          enumValueTEF[A](e)
        }
      }

    // =========================== Numerical Integral ===========================

    val byteTEF: IntegralTypedExpressionFactory[Byte, TByte, Float, TFloat] with PrimitiveJdbcMapper[Byte] =
      new IntegralTypedExpressionFactory[Byte, TByte, Float, TFloat] with PrimitiveJdbcMapper[Byte] {
        val sample: Byte = 1: Byte
        val defaultColumnLength = 1
        val floatifyer: TypedExpressionFactory[Float, TFloat] = floatTEF

        def extractNativeJdbcValue(rs: ResultSet, i: Int): Byte = rs.getByte(i)
      }

    val optionByteTEF: IntegralTypedExpressionFactory[Option[Byte], TOptionByte, Option[Float], TOptionFloat]
      with DeOptionizer[Byte, Byte, TByte, Option[Byte], TOptionByte] =
      new IntegralTypedExpressionFactory[Option[Byte], TOptionByte, Option[Float], TOptionFloat]
        with DeOptionizer[Byte, Byte, TByte, Option[Byte], TOptionByte] {
        val deOptionizer: TypedExpressionFactory[Byte, TByte] with JdbcMapper[Byte, Byte] = byteTEF
        val floatifyer: TypedExpressionFactory[Option[Float], TOptionFloat] = optionFloatTEF
      }

    val intTEF: IntegralTypedExpressionFactory[Int, TInt, Float, TFloat] with PrimitiveJdbcMapper[Int] =
      new IntegralTypedExpressionFactory[Int, TInt, Float, TFloat] with PrimitiveJdbcMapper[Int] {
        val sample = 1
        val defaultColumnLength = 4
        val floatifyer: TypedExpressionFactory[Float, TFloat] = floatTEF

        def extractNativeJdbcValue(rs: ResultSet, i: Int): Int = rs.getInt(i)
      }

    val optionIntTEF: IntegralTypedExpressionFactory[Option[Int], TOptionInt, Option[Float], TOptionFloat]
      with DeOptionizer[Int, Int, TInt, Option[Int], TOptionInt] =
      new IntegralTypedExpressionFactory[Option[Int], TOptionInt, Option[Float], TOptionFloat]
        with DeOptionizer[Int, Int, TInt, Option[Int], TOptionInt] {
        val deOptionizer: TypedExpressionFactory[Int, TInt] with JdbcMapper[Int, Int] = intTEF
        val floatifyer: TypedExpressionFactory[Option[Float], TOptionFloat] = optionFloatTEF
      }

    val longTEF: IntegralTypedExpressionFactory[Long, TLong, Double, TDouble] with PrimitiveJdbcMapper[Long] =
      new IntegralTypedExpressionFactory[Long, TLong, Double, TDouble] with PrimitiveJdbcMapper[Long] {
        val sample = 1L
        val defaultColumnLength = 8
        val floatifyer: TypedExpressionFactory[Double, TDouble] = doubleTEF

        def extractNativeJdbcValue(rs: ResultSet, i: Int): Long = rs.getLong(i)
      }

    val optionLongTEF: IntegralTypedExpressionFactory[Option[Long], TOptionLong, Option[Double], TOptionDouble]
      with DeOptionizer[Long, Long, TLong, Option[Long], TOptionLong] =
      new IntegralTypedExpressionFactory[Option[Long], TOptionLong, Option[Double], TOptionDouble]
        with DeOptionizer[Long, Long, TLong, Option[Long], TOptionLong] {
        val deOptionizer: TypedExpressionFactory[Long, TLong] with JdbcMapper[Long, Long] = longTEF
        val floatifyer: TypedExpressionFactory[Option[Double], TOptionDouble] = optionDoubleTEF
      }

    // =========================== Numerical Floating Point ===========================

    val floatTEF: FloatTypedExpressionFactory[Float, TFloat] with PrimitiveJdbcMapper[Float] =
      new FloatTypedExpressionFactory[Float, TFloat] with PrimitiveJdbcMapper[Float] {
        val sample = 1f
        val defaultColumnLength = 4

        def extractNativeJdbcValue(rs: ResultSet, i: Int): Float = rs.getFloat(i)
      }

    val optionFloatTEF: FloatTypedExpressionFactory[Option[Float], TOptionFloat]
      with DeOptionizer[Float, Float, TFloat, Option[Float], TOptionFloat] =
      new FloatTypedExpressionFactory[Option[Float], TOptionFloat]
        with DeOptionizer[Float, Float, TFloat, Option[Float], TOptionFloat] {
        val deOptionizer: TypedExpressionFactory[Float, TFloat] with JdbcMapper[Float, Float] = floatTEF
      }

    val doubleTEF: FloatTypedExpressionFactory[Double, TDouble] with PrimitiveJdbcMapper[Double] =
      new FloatTypedExpressionFactory[Double, TDouble] with PrimitiveJdbcMapper[Double] {
        val sample = 1d
        val defaultColumnLength = 8

        def extractNativeJdbcValue(rs: ResultSet, i: Int): Double = rs.getDouble(i)
      }

    val optionDoubleTEF: FloatTypedExpressionFactory[Option[Double], TOptionDouble]
      with DeOptionizer[Double, Double, TDouble, Option[Double], TOptionDouble] =
      new FloatTypedExpressionFactory[Option[Double], TOptionDouble]
        with DeOptionizer[Double, Double, TDouble, Option[Double], TOptionDouble] {
        val deOptionizer: TypedExpressionFactory[Double, TDouble] with JdbcMapper[Double, Double] = doubleTEF
      }

    val bigDecimalTEF: FloatTypedExpressionFactory[BigDecimal, TBigDecimal] with PrimitiveJdbcMapper[BigDecimal] =
      new FloatTypedExpressionFactory[BigDecimal, TBigDecimal] with PrimitiveJdbcMapper[BigDecimal] {
        val sample: BigDecimal = BigDecimal(1)
        val defaultColumnLength: Int = -1

        def extractNativeJdbcValue(rs: ResultSet, i: Int): BigDecimal = {
          val v = rs.getBigDecimal(i)
          if (rs.wasNull())
            null
          else
            BigDecimal(v)
        }
      }

    val optionBigDecimalTEF: FloatTypedExpressionFactory[Option[BigDecimal], TOptionBigDecimal]
      with DeOptionizer[BigDecimal, BigDecimal, TBigDecimal, Option[BigDecimal], TOptionBigDecimal] =
      new FloatTypedExpressionFactory[Option[BigDecimal], TOptionBigDecimal]
        with DeOptionizer[BigDecimal, BigDecimal, TBigDecimal, Option[BigDecimal], TOptionBigDecimal] {
        val deOptionizer: TypedExpressionFactory[BigDecimal, TBigDecimal] with JdbcMapper[BigDecimal, BigDecimal] =
          bigDecimalTEF
      }
  }

  initialize

  protected def initialize(): Option[FieldAttributesBasedOnType[_]] = {
    import PrimitiveTypeSupport._

    register(byteTEF)
    register(intTEF)
    register(longTEF)
    register(floatTEF)
    register(doubleTEF)
    register(bigDecimalTEF)
    register(binaryTEF)
    register(booleanTEF)
    register(stringTEF)
    register(timestampTEF)
    register(localDateTimeTEF)
    register(offsetTimeTEF)
    register(offsetDateTimeTEF)
    register(dateTEF)
    register(sqlDateTEF)
    register(localDateTEF)
    register(localTimeTEF)
    register(uuidTEF)
    register(intArrayTEF)
    register(longArrayTEF)
    register(doubleArrayTEF)
    register(stringArrayTEF)

    val re: JdbcMapper[Int, Enumeration#Value]
      with TypedExpressionFactory[Enumeration#Value, TEnumValue[Enumeration#Value]] = enumValueTEF(
      DummyEnum.DummyEnumerationValue
    )

    /**
     * Enumerations are treated differently, since the map method should normally
     * return the actual Enumeration#value, but given that an enum is not only
     * determined by the int value from the DB, but also the parent Enumeration
     * parentEnumeration.values.find(_.id == v), the conversion is done
     * in FieldMetaData.canonicalEnumerationValueFor(i: Int)
     */
    val z = new FieldAttributesBasedOnType[Any](
      new {
        def map(rs: ResultSet, i: Int): Int = rs.getInt(i)

        def convertToJdbc(v: AnyRef): AnyRef = v
      },
      re.defaultColumnLength,
      re.sample,
      classOf[java.lang.Integer]
    )

    registry.put(z.clasz, z)
    registry.put(z.clasz.getSuperclass, z)
  }

  protected trait MapperForReflection {
    def map(rs: ResultSet, i: Int): Any
    def convertToJdbc(v: AnyRef): AnyRef
  }

  protected def makeMapper(fa0: JdbcMapper[_, _]) = new MapperForReflection {
    val fa = fa0.asInstanceOf[JdbcMapper[AnyRef, AnyRef]]

    def map(rs: ResultSet, i: Int): AnyRef = fa.map(rs, i)

    def convertToJdbc(v: AnyRef): AnyRef = {
      if (v != null)
        fa.convertToJdbc(v)
      else null
    }
  }

  protected class FieldAttributesBasedOnType[A](
    val mapper: MapperForReflection,
    val defaultLength: Int,
    val sample: A,
    val nativeJdbcType: Class[_]
  ) {

    val clasz: Class[_] = {
      val sampleClass = sample.asInstanceOf[AnyRef].getClass
      if (classOf[TargetsValuesSupertype].isAssignableFrom(sampleClass)) sampleClass.getSuperclass else sampleClass
    }

    override def toString: String =
      clasz.getCanonicalName + " --> " + mapper.getClass.getCanonicalName
  }

  def nativeJdbcValueFor(nonNativeType: Class[_], r: AnyRef): AnyRef =
    get(nonNativeType).mapper.convertToJdbc(r)

  def isSupported(c: Class[_]): Boolean =
    lookup(c).isDefined ||
      c.isAssignableFrom(classOf[Some[_]]) ||
      classOf[Product1[Any]].isAssignableFrom(c)

  def defaultColumnLength(c: Class[_]): Int =
    get(c).defaultLength

  def nativeJdbcTypeFor(c: Class[_]): Class[_] =
    get(c).nativeJdbcType

  def resultSetHandlerFor(c: Class[_]): (ResultSet, Int) => AnyRef = {
    val fa = get(c)
    (rs: ResultSet, i: Int) => {
      val z = fa.mapper.map(rs, i)
      if (rs.wasNull) null
      else z.asInstanceOf[AnyRef]
    }
  }

  private def get(c: Class[_]) =
    lookup(c).getOrElse(
      Utils.throwError(
        "Usupported native type " + c.getCanonicalName + "," + c.getName + "\n" + registry.mkString("\n")
      )
    )

  def sampleValueFor(c: Class[_]): AnyRef =
    get(c).sample.asInstanceOf[AnyRef]

  def trySampleValueFor(c: Class[_]): AnyRef = {
    val r = lookup(c).map(_.sample)
    r match {
      case Some(x: AnyRef) => x
      case _ => null
    }
  }

  private[squeryl] def register[P, A](m: NonPrimitiveJdbcMapper[P, A, _]): Unit = {

    val z =
      new FieldAttributesBasedOnType(makeMapper(m), m.defaultColumnLength, m.sample, m.primitiveMapper.nativeJdbcType)

    val wasThere = registry.put(z.clasz, z)

    if (wasThere.isDefined)
      Utils.throwError("field type " + z.clasz + " already registered, handled by " + m.getClass.getCanonicalName)
  }

  def register[S, J](m: ArrayJdbcMapper[S, J]): Unit = {
    val f = m.thisTypedExpressionFactory
    val z = new FieldAttributesBasedOnType(makeMapper(m), m.defaultColumnLength, f.sample, m.nativeJdbcType)

    val wasThere = registry.put(z.clasz, z)

    if (wasThere.isDefined)
      Utils.throwError("field type " + z.clasz + " already registered, handled by " + m.getClass.getCanonicalName)
  }

  private def register[A](pm: PrimitiveJdbcMapper[A]): Unit = {
    val f = pm.thisTypedExpressionFactory
    val z = new FieldAttributesBasedOnType(makeMapper(pm), f.defaultColumnLength, f.sample, pm.nativeJdbcType)

    val c = z.clasz

    registry.put(c, z)
  }

  @tailrec
  private def lookup(c: Class[_]): Option[FieldAttributesBasedOnType[_]] = {
    if (!c.isPrimitive)
      registry.get(c)
    else
      c.getName match {
        case "int" => lookup(classOf[java.lang.Integer])
        case "long" => lookup(classOf[java.lang.Long])
        case "float" => lookup(classOf[java.lang.Float])
        case "byte" => lookup(classOf[java.lang.Byte])
        case "boolean" => lookup(classOf[java.lang.Boolean])
        case "double" => lookup(classOf[java.lang.Double])
        case "void" => None
      }
  }
}
