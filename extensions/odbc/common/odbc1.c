/*================================================================ 
          odbc1.c
          --Generated from: odbc.src
          Date: 1999/4/9   Time: 19:15:51
		--by ALS Interface Generator

 *===============================================================*/

#include "alspi.h"
#include "cinterf.h"
#include "odbc.h"


CI_BEGARRAY(fieldsDATE_STRUCT)
  CI_FIELD("year",year,DATE_STRUCT,8,""),
  CI_FIELD("month",month,DATE_STRUCT,8,""),
  CI_FIELD("day",day,DATE_STRUCT,8,""),
  {0}
CI_ENDARRAY

CI_BEGARRAY(fieldsSQL_DATE_STRUCT)
  CI_FIELD("year",year,SQL_DATE_STRUCT,8,""),
  CI_FIELD("month",month,SQL_DATE_STRUCT,8,""),
  CI_FIELD("day",day,SQL_DATE_STRUCT,8,""),
  {0}
CI_ENDARRAY

CI_BEGARRAY(fieldsTIME_STRUCT)
  CI_FIELD("hour",hour,TIME_STRUCT,8,""),
  CI_FIELD("minute",minute,TIME_STRUCT,8,""),
  CI_FIELD("second",second,TIME_STRUCT,8,""),
  {0}
CI_ENDARRAY

CI_BEGARRAY(fieldsSQL_TIME_STRUCT)
  CI_FIELD("hour",hour,SQL_TIME_STRUCT,8,""),
  CI_FIELD("minute",minute,SQL_TIME_STRUCT,8,""),
  CI_FIELD("second",second,SQL_TIME_STRUCT,8,""),
  {0}
CI_ENDARRAY

CI_BEGARRAY(fieldsTIMESTAMP_STRUCT)
  CI_FIELD("year",year,TIMESTAMP_STRUCT,8,""),
  CI_FIELD("month",month,TIMESTAMP_STRUCT,8,""),
  CI_FIELD("day",day,TIMESTAMP_STRUCT,8,""),
  CI_FIELD("hour",hour,TIMESTAMP_STRUCT,8,""),
  CI_FIELD("minute",minute,TIMESTAMP_STRUCT,8,""),
  CI_FIELD("second",second,TIMESTAMP_STRUCT,8,""),
  CI_FIELD("fraction",fraction,TIMESTAMP_STRUCT,3,""),
  {0}
CI_ENDARRAY

CI_BEGARRAY(fieldsSQL_TIMESTAMP_STRUCT)
  CI_FIELD("year",year,SQL_TIMESTAMP_STRUCT,8,""),
  CI_FIELD("month",month,SQL_TIMESTAMP_STRUCT,8,""),
  CI_FIELD("day",day,SQL_TIMESTAMP_STRUCT,8,""),
  CI_FIELD("hour",hour,SQL_TIMESTAMP_STRUCT,8,""),
  CI_FIELD("minute",minute,SQL_TIMESTAMP_STRUCT,8,""),
  CI_FIELD("second",second,SQL_TIMESTAMP_STRUCT,8,""),
  CI_FIELD("fraction",fraction,SQL_TIMESTAMP_STRUCT,3,""),
  {0}
CI_ENDARRAY

CI_BEGARRAY(fieldsSQL_YEAR_MONTH_STRUCT)
  CI_FIELD("year",year,SQL_YEAR_MONTH_STRUCT,3,""),
  CI_FIELD("month",month,SQL_YEAR_MONTH_STRUCT,3,""),
  {0}
CI_ENDARRAY

CI_BEGARRAY(fieldsSQL_DAY_SECOND_STRUCT)
  CI_FIELD("day",day,SQL_DAY_SECOND_STRUCT,3,""),
  CI_FIELD("hour",hour,SQL_DAY_SECOND_STRUCT,3,""),
  CI_FIELD("minute",minute,SQL_DAY_SECOND_STRUCT,3,""),
  CI_FIELD("second",second,SQL_DAY_SECOND_STRUCT,3,""),
  CI_FIELD("fraction",fraction,SQL_DAY_SECOND_STRUCT,3,""),
  {0}
CI_ENDARRAY

CI_BEGARRAY(fieldsSQL_INTERVAL_STRUCT)
  CI_FIELD("interval_type",interval_type,SQL_INTERVAL_STRUCT,1,""),
  CI_FIELD("interval_sign",interval_sign,SQL_INTERVAL_STRUCT,8,""),
  CI_FIELD("intval.year_month",intval.year_month,SQL_INTERVAL_STRUCT,0,"SQL_YEAR_MONTH_STRUCT"),
  CI_FIELD("intval.day_second",intval.day_second,SQL_INTERVAL_STRUCT,0,"SQL_DAY_SECOND_STRUCT"),
  {0}
CI_ENDARRAY

CI_BEGARRAY(fieldsSQL_NUMERIC_STRUCT)
  CI_FIELD("precision",precision,SQL_NUMERIC_STRUCT,6,""),
  CI_FIELD("scale",scale,SQL_NUMERIC_STRUCT,6,""),
  CI_FIELD("sign",sign,SQL_NUMERIC_STRUCT,6,""),
  CI_ARRAYFIELD("val",val,SQL_NUMERIC_STRUCT,6,"",16),
  {0}
CI_ENDARRAY

void odbc2_init(void)
{
  /*
  Not defined in unixODBC headers
  CI_CTYPEDEF("SQLTCHAR",SQLTCHAR,6)
  CI_CTYPEDEF("SQLWCHAR",SQLWCHAR,8)
  */
  CI_CTYPEDEF("BOOKMARK",BOOKMARK,3)
  CI_STRUCT("SQL_NUMERIC_STRUCT",SQL_NUMERIC_STRUCT,fieldsSQL_NUMERIC_STRUCT)
  CI_CTYPEDEF("SQLUBIGINT",SQLUBIGINT,1)
  /*
  Not defined in unixODBC headers
  CI_CTYPEDEF("__int64",__int64,0)
  */
  CI_STRUCT("SQL_INTERVAL_STRUCT",SQL_INTERVAL_STRUCT,fieldsSQL_INTERVAL_STRUCT)
  CI_STRUCT("SQL_DAY_SECOND_STRUCT",SQL_DAY_SECOND_STRUCT,fieldsSQL_DAY_SECOND_STRUCT)
  CI_STRUCT("SQL_YEAR_MONTH_STRUCT",SQL_YEAR_MONTH_STRUCT,fieldsSQL_YEAR_MONTH_STRUCT)
  CI_CTYPEDEF("SQLINTERVAL",SQLINTERVAL,1)
  CI_STRUCT("SQL_TIMESTAMP_STRUCT",SQL_TIMESTAMP_STRUCT,fieldsSQL_TIMESTAMP_STRUCT)
  CI_STRUCT("TIMESTAMP_STRUCT",TIMESTAMP_STRUCT,fieldsTIMESTAMP_STRUCT)
  CI_STRUCT("SQL_TIME_STRUCT",SQL_TIME_STRUCT,fieldsSQL_TIME_STRUCT)
  CI_STRUCT("TIME_STRUCT",TIME_STRUCT,fieldsTIME_STRUCT)
  CI_STRUCT("SQL_DATE_STRUCT",SQL_DATE_STRUCT,fieldsSQL_DATE_STRUCT)
  CI_STRUCT("DATE_STRUCT",DATE_STRUCT,fieldsDATE_STRUCT)
  CI_CTYPEDEF("SQLHWND",SQLHWND,5)
  CI_CTYPEDEF("RETCODE",RETCODE,8)
  CI_CTYPEDEF("HSTMT",HSTMT,5)
  CI_CTYPEDEF("HDBC",HDBC,5)
  CI_CTYPEDEF("HENV",HENV,5)
  CI_CTYPEDEF("PTR",PTR,5)
  CI_CTYPEDEF("SFLOAT",SFLOAT,12)
  CI_CTYPEDEF("LDOUBLE",LDOUBLE,13)
  CI_CTYPEDEF("SDOUBLE",SDOUBLE,13)
  CI_CTYPEDEF("USHORT",USHORT,8)
  CI_CTYPEDEF("ULONG",ULONG,3)
  CI_CTYPEDEF("SSHORT",SSHORT,8)
  CI_CTYPEDEF("SLONG",SLONG,3)
  CI_CTYPEDEF("UWORD",UWORD,8)
  CI_CTYPEDEF("UDWORD",UDWORD,3)
  CI_CTYPEDEF("SWORD",SWORD,8)
  CI_CTYPEDEF("SDWORD",SDWORD,3)
  CI_CTYPEDEF("SCHAR",SCHAR,6)
  CI_CTYPEDEF("UCHAR",UCHAR,6)
  CI_CTYPEDEF("SQLHDESC",SQLHDESC,3)
  CI_CTYPEDEF("SQLHSTMT",SQLHSTMT,3)
  CI_CTYPEDEF("SQLHDBC",SQLHDBC,3)
  CI_CTYPEDEF("SQLHENV",SQLHENV,3)
  CI_CTYPEDEF("SQLHANDLE",SQLHANDLE,3)
  CI_CTYPEDEF("SQLRETURN",SQLRETURN,8)
  CI_CTYPEDEF("SQLVARCHAR",SQLVARCHAR,6)
  CI_CTYPEDEF("SQLTIMESTAMP",SQLTIMESTAMP,6)
  CI_CTYPEDEF("SQLTIME",SQLTIME,6)
  CI_CTYPEDEF("SQLUSMALLINT",SQLUSMALLINT,8)
  CI_CTYPEDEF("SQLSMALLINT",SQLSMALLINT,8)
  CI_CTYPEDEF("SQLREAL",SQLREAL,12)
  CI_CTYPEDEF("SQLPOINTER",SQLPOINTER,5)
  CI_CTYPEDEF("SQLNUMERIC",SQLNUMERIC,6)
  CI_CTYPEDEF("SQLUINTEGER",SQLUINTEGER,3)
  CI_CTYPEDEF("SQLINTEGER",SQLINTEGER,3)
  CI_CTYPEDEF("SQLFLOAT",SQLFLOAT,13)
  CI_CTYPEDEF("SQLDOUBLE",SQLDOUBLE,13)
  CI_CTYPEDEF("SQLDECIMAL",SQLDECIMAL,6)
  CI_CTYPEDEF("SQLDATE",SQLDATE,6)
  CI_CTYPEDEF("SQLSCHAR",SQLSCHAR,6)
  CI_CTYPEDEF("SQLCHAR",SQLCHAR,6)
  CI_CTYPEDEF("HWND",HWND,5)
  CI_CTYPEDEF("ptr",void *,5)
  CI_CTYPEDEF("double",double,13)
  CI_CTYPEDEF("float",float,12)
  CI_CTYPEDEF("short",short,8)
  CI_CTYPEDEF("char",char,6)
  CI_CTYPEDEF("long",long,3)
  CI_CTYPEDEF("int",int,1)
}
