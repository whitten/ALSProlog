#include "Types.r"

resource 'FREF' (128) {
	'APPL',
	0,
	""
};

resource 'FREF' (129) {
	'TEXT',
	1,
	""
};

resource 'FREF' (130) {
	'OBPT',
	2,
	""
};

resource 'BNDL' (128) {
	'ALS4',
	0,
	{	/* array TypeArray: 6 elements */
		/* [1] */
		'FREF',
		{	/* array IDArray: 3 elements */
			/* [1] */
			0, 128,
			/* [2] */
			1, 129,
			/* [3] */
			2, 130
		},
		/* [2] */
		'ICN#',
		{	/* array IDArray: 3 elements */
			/* [1] */
			0, 128,
			/* [2] */
			1, 129,
			/* [3] */
			2, 130
		}
	}
};

resource 'open' (128)
{
	'ALS4', { }
};

/* ftApplicationName should be defined in Types.r, but it isn't.
   The Mac Easy Open SDK uses FileTypesAndCreators.h to get this
   defined, but the latest version causes Rez errors - so I'll
   define it here for now.

   Same story for verUS.
*/
#ifndef ftApplicationName
#define ftApplicationName	'apnm'
#endif

#ifndef verUS
#define verUS	0
#endif

resource 'kind' (1000)
{
	'ALS4',
	verUS,
	{
		ftApplicationName,	"ALS Prolog",
		'TEXT',			"ALS Prolog text file",
		'OBPT',			"ALS Prolog object file"
	}
};

data 'ALS4' (0, "Owner resource") {
	$"21A9 2031 3939 3520 4170 706C 6965 6420"            /* !� 1995 Applied  */
	$"4C6F 6769 6320 5379 7374 656D 7320 496E"            /* Logic Systems In */
	$"632E"                                               /* c. */
};


/* Icons */

resource 'ICN#' (128) {
	{	/* array: 2 elements */
		/* [1] */
		$"FFFF FF80 8000 AFC0 8000 55E0 8001 16F0"
		$"8000 2978 8000 06DC 83FF FFFE 8300 0002"
		$"8280 0002 8240 0002 8220 0002 8210 0002"
		$"828F FF82 8348 3F82 82A8 2D82 83D8 3682"
		$"83B8 2582 83E8 2A82 83FF E082 8000 1282"
		$"8000 0882 8000 0482 8000 0282 8000 0182"
		$"FFFF FF82 7D40 0002 3F94 0002 1AD0 0002"
		$"0F65 0002 07B2 0002 03FF FFFE",
		/* [2] */
		$"FFFF FF80 FFFF FFC0 FFFF FFE0 FFFF FFF0"
		$"FFFF FFF8 FFFF FFFC FFFF FFFE FFFF FFFE"
		$"FFFF FFFE FFFF FFFE FFFF FFFE FFFF FFFE"
		$"FFFF FFFE FFF8 3FFE FFF8 3FFE FFF8 3FFE"
		$"FFF8 3FFE FFF8 3FFE FFFF FFFE FFFF FFFE"
		$"FFFF FFFE FFFF FFFE FFFF FFFE FFFF FFFE"
		$"FFFF FFFE 7FFF FFFE 3FFF FFFE 1FFF FFFE"
		$"0FFF FFFE 07FF FFFE 03FF FFFE"
	}
};

resource 'ICN#' (129) {
	{	/* array: 2 elements */
		/* [1] */
		$"1FFF FC00 1000 0600 17FC 0500 140E 0480"
		$"15FF 0440 1581 0420 157D 07F0 15DD 0010"
		$"15F5 0010 140D 0010 17FD 0010 1381 0010"
		$"11FF 0010 1000 0010 1200 0010 10C0 0010"
		$"1200 0010 1000 0010 1000 0010 13B5 BE10"
		$"1000 0010 1376 E810 1000 0010 13DB 5810"
		$"1000 0010 136F 7010 1000 0010 13DC F410"
		$"1000 0010 1000 0010 1000 0010 1FFF FFF0",
		/* [2] */
		$"1FFF FC00 1FFF FE00 1FFF FF00 1FFF FF80"
		$"1FFF FFC0 1FFF FFE0 1FFF FFF0 1FFF FFF0"
		$"1FFF FFF0 1FFF FFF0 1FFF FFF0 1FFF FFF0"
		$"1FFF FFF0 1FFF FFF0 1FFF FFF0 1FFF FFF0"
		$"1FFF FFF0 1FFF FFF0 1FFF FFF0 1FFF FFF0"
		$"1FFF FFF0 1FFF FFF0 1FFF FFF0 1FFF FFF0"
		$"1FFF FFF0 1FFF FFF0 1FFF FFF0 1FFF FFF0"
		$"1FFF FFF0 1FFF FFF0 1FFF FFF0 1FFF FFF0"
	}
};

resource 'ICN#' (130) {
	{	/* array: 2 elements */
		/* [1] */
		$"1FFF FC00 1000 0600 17FC 0500 140E 0480"
		$"15FF 0440 1581 0420 157D 07F0 15DD 0010"
		$"15F5 0010 140D 0010 17FD 0010 1381 0010"
		$"11FF 0010 1000 0010 1200 0010 10C0 0010"
		$"1200 0010 1000 0010 1000 0010 108C 4610"
		$"1192 C910 1092 4910 108C 4610 1000 0010"
		$"1084 6210 118C 9610 1084 9210 1084 6210"
		$"1000 0010 1000 0010 1000 0010 1FFF FFF0",
		/* [2] */
		$"1FFF FC00 1FFF FE00 1FFF FF00 1FFF FF80"
		$"1FFF FFC0 1FFF FFE0 1FFF FFF0 1FFF FFF0"
		$"1FFF FFF0 1FFF FFF0 1FFF FFF0 1FFF FFF0"
		$"1FFF FFF0 1FFF FFF0 1FFF FFF0 1FFF FFF0"
		$"1FFF FFF0 1FFF FFF0 1FFF FFF0 1FFF FFF0"
		$"1FFF FFF0 1FFF FFF0 1FFF FFF0 1FFF FFF0"
		$"1FFF FFF0 1FFF FFF0 1FFF FFF0 1FFF FFF0"
		$"1FFF FFF0 1FFF FFF0 1FFF FFF0 1FFF FFF0"
	}
};

resource 'ics#' (128) {
	{	/* array: 2 elements */
		/* [1] */
		$"FFF8 80BC 805E 9FFF 9801 9401 93F9 9A79"
		$"9E59 9FC9 8029 8019 FFF9 F401 7A01 3FFF",
		/* [2] */
		$"FFF8 FFFC FFFE FFFF FFFF FFFF FFFF FE7F"
		$"FE7F FFFF FFFF FFFF FFFF 7FFF 3FFF 1FFF"
	}
};

resource 'ics#' (129) {
	{	/* array: 2 elements */
		/* [1] */
		$"7FE0 4030 5E28 553C 5B04 5504 4F04 4004"
		$"5004 4604 5004 42C4 4004 43A4 4004 7FFC",
		/* [2] */
		$"7FE0 7FF0 7FF8 7FFC 7FFC 7FFC 7FFC 7FFC"
		$"7FFC 7FFC 7FFC 7FFC 7FFC 7FFC 7FFC 7FFC"
	}
};

resource 'ics#' (130) {
	{	/* array: 2 elements */
		/* [1] */
		$"7FE0 4030 5E28 553C 5B04 5504 4F04 4004"
		$"4004 44C4 4D24 4524 44C4 4004 4004 7FFC",
		/* [2] */
		$"7FE0 7FF0 7FF8 7FFC 7FFC 7FFC 7FFC 7FFC"
		$"7FFC 7FFC 7FFC 7FFC 7FFC 7FFC 7FFC 7FFC"
	}
};

resource 'icl8' (128) {
	$"EDED EDED EDED EDED EDED EDED EDED EDED"
	$"EDED EDED EDED EDED ED00 0000 0000 0000"
	$"EDF5 F5F5 F5F5 F5F5 F5F5 F5F5 F52A 2A54"
	$"787E A8A8 A8A8 A8ED EDED 0000 0000 0000"
	$"EDF5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 2A2A"
	$"5478 7EA8 A8A8 A8A8 EDED ED00 0000 0000"
	$"EDF5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F52A"
	$"2A54 787E A8A8 A8A8 A8ED EDED 0000 0000"
	$"EDF5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
	$"2A2A 5478 7EA8 A8A8 A8A8 EDED ED00 0000"
	$"EDF5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
	$"F52A 2A54 787E A8A8 A8A8 A8ED EDED 0000"
	$"EDF5 F5F5 F5F5 EDED EDED EDED EDED EDED"
	$"EDED EDED EDED EDED EDED EDED EDED ED00"
	$"EDF5 F5F5 F5F5 EDED F5F5 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 ED00"
	$"EDF5 F5F5 F5F5 EDF5 EDF5 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 ED00"
	$"EDF5 F5F5 F5F5 EDF5 F5ED F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 ED00"
	$"EDF5 F5F5 F5F5 ED2A F5F5 EDF5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 ED00"
	$"EDF5 F5F5 F5F5 ED2A 2A2A F5ED F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 ED00"
	$"EDF5 F5F5 F5F5 ED54 542A 2AF5 EDED EDED"
	$"EDED EDED EDED EDED EDF5 F5F5 F5F5 ED00"
	$"EDF5 F5F5 F5F5 ED7E 7854 2A2A ED00 0000"
	$"0000 EDEB EBEB EBED EDF5 F5F5 F5F5 ED00"
	$"EDF5 F5F5 F5F5 EDA8 7E78 782A ED00 0000"
	$"0000 EDA8 D2D2 D2EB EDF5 F5F5 F5F5 ED00"
	$"EDF5 F5F5 F5F5 EDD2 A8A8 A878 ED00 0000"
	$"0000 ED78 A8A8 A8D2 EDF5 F5F5 F5F5 ED00"
	$"EDF5 F5F5 F5F5 EDEB D2D2 D2A8 ED00 0000"
	$"0000 ED2A 7878 7EA8 EDF5 F5F5 F5F5 ED00"
	$"EDF5 F5F5 F5F5 EDED EBEB EBEB ED00 0000"
	$"0000 ED2A 2A54 787E EDF5 F5F5 F5F5 ED00"
	$"EDF5 F5F5 F5F5 EDED EDED EDED EDED EDED"
	$"EDED EDF5 2A2A 5454 EDF5 F5F5 F5F5 ED00"
	$"EDF5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
	$"F5F5 F5ED F52A 2A2A EDF5 F5F5 F5F5 ED00"
	$"EDF5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 EDF5 F52A EDF5 F5F5 F5F5 ED00"
	$"EDF5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5ED F5F5 EDF5 F5F5 F5F5 ED00"
	$"EDF5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 EDF5 EDF5 F5F5 F5F5 ED00"
	$"EDF5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5ED EDF5 F5F5 F5F5 ED00"
	$"EDED EDED EDED EDED EDED EDED EDED EDED"
	$"EDED EDED EDED EDED EDF5 F5F5 F5F5 ED00"
	$"00ED EDD3 EBD2 D2A8 A87E 7854 2A2A F5F5"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 ED00"
	$"0000 EDED D3EB D2D2 A8A8 7E78 542A 2AF5"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 ED00"
	$"0000 00ED EDD3 EBD2 D2A8 A87E 7854 2A2A"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 ED00"
	$"0000 0000 EDED D3EB D2D2 A8A8 7E78 542A"
	$"2AF5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 ED00"
	$"0000 0000 00ED EDD3 EBD2 D2A8 A87E 7854"
	$"2A2A F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 ED00"
	$"0000 0000 0000 EDED EDED EDED EDED EDED"
	$"EDED EDED EDED EDED EDED EDED EDED ED"
};

resource 'icl8' (129) {
	$"0000 00FF FFFF FFFF FFFF FFFF FFFF FFFF"
	$"FFFF FFFF FFFF 0000 0000 0000 0000 0000"
	$"0000 00FF F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5FF FF00 0000 0000 0000 0000"
	$"0000 00FF F5ED EDED EDED EDED EDED F5F5"
	$"F5F5 F5F5 F5FF 2BFF 0000 0000 0000 0000"
	$"0000 00FF F5ED F5F5 F5F5 F578 A8ED EDF5"
	$"F5F5 F5F5 F5FF 2B2B FF00 0000 0000 0000"
	$"0000 00FF F5ED F5ED EDED EDED EDED EDED"
	$"F5F5 F5F5 F5FF 2B2B 2BFF 0000 0000 0000"
	$"0000 00FF F5ED F5ED EDF5 F5F5 F5F5 F5ED"
	$"F5F5 F5F5 F5FF 2B2B 2B2B FF00 0000 0000"
	$"0000 00FF F5ED F5ED F5ED EDED EDED F5ED"
	$"F5F5 F5F5 F5FF FFFF FFFF FFFF 0000 0000"
	$"0000 00FF F5ED F5ED A8ED 00ED A8ED F5ED"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000 0000"
	$"0000 00FF F5ED F5ED EDED EDED F5ED F5ED"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000 0000"
	$"0000 00FF F5ED F5F5 F5F5 F5F5 EDED F5ED"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000 0000"
	$"0000 00FF F5ED EDED EDED EDED EDED F5ED"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000 0000"
	$"0000 00FF F5F5 EDED A878 2AF5 F5F5 F5ED"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000 0000"
	$"0000 00FF F5F5 F5ED EDED EDED EDED EDED"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000 0000"
	$"0000 00FF F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000 0000"
	$"0000 00FF F5F5 EDF5 F5F5 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000 0000"
	$"0000 00FF F5F5 F5F5 EDED F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000 0000"
	$"0000 00FF F5F5 EDF5 F5F5 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000 0000"
	$"0000 00FF F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000 0000"
	$"0000 00FF F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000 0000"
	$"0000 00FF F5F5 EDED EDF5 EDED F5ED F5ED"
	$"EDF5 EDED EDED EDF5 F5F5 F5FF 0000 0000"
	$"0000 00FF F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000 0000"
	$"0000 00FF F5F5 EDED F5ED EDED F5ED EDF5"
	$"EDED EDF5 EDF5 F5F5 F5F5 F5FF 0000 0000"
	$"0000 00FF F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000 0000"
	$"0000 00FF F5F5 EDED EDED F5ED EDF5 EDED"
	$"F5ED F5ED EDF5 F5F5 F5F5 F5FF 0000 0000"
	$"0000 00FF F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000 0000"
	$"0000 00FF F5F5 EDED F5ED EDF5 EDED EDED"
	$"F5ED EDED F5F5 F5F5 F5F5 F5FF 0000 0000"
	$"0000 00FF F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000 0000"
	$"0000 00FF F5F5 EDED EDED F5ED EDED F5F5"
	$"EDED EDED F5ED F5F5 F5F5 F5FF 0000 0000"
	$"0000 00FF F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000 0000"
	$"0000 00FF F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000 0000"
	$"0000 00FF F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000 0000"
	$"0000 00FF FFFF FFFF FFFF FFFF FFFF FFFF"
	$"FFFF FFFF FFFF FFFF FFFF FFFF"
};

resource 'icl8' (130) {
	$"0000 00FF FFFF FFFF FFFF FFFF FFFF FFFF"
	$"FFFF FFFF FFFF 0000 0000 0000 0000 0000"
	$"0000 00FF F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5FF FF00 0000 0000 0000 0000"
	$"0000 00FF F5ED EDED EDED EDED EDED F5F5"
	$"F5F5 F5F5 F5FF 2BFF 0000 0000 0000 0000"
	$"0000 00FF F5ED F5F5 F5F5 F578 A8ED EDF5"
	$"F5F5 F5F5 F5FF 2B2B FF00 0000 0000 0000"
	$"0000 00FF F5ED F5ED EDED EDED EDED EDED"
	$"F5F5 F5F5 F5FF 2B2B 2BFF 0000 0000 0000"
	$"0000 00FF F5ED F5ED EDF5 F5F5 F5F5 F5ED"
	$"F5F5 F5F5 F5FF 2B2B 2B2B FF00 0000 0000"
	$"0000 00FF F5ED F5ED F5ED EDED EDED F5ED"
	$"F5F5 F5F5 F5FF FFFF FFFF FFFF 0000 0000"
	$"0000 00FF F5ED F5ED A8ED 00ED A8ED F5ED"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000 0000"
	$"0000 00FF F5ED F5ED EDED EDED F5ED F5ED"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000 0000"
	$"0000 00FF F5ED F5F5 F5F5 F5F5 EDED F5ED"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000 0000"
	$"0000 00FF F5ED EDED EDED EDED EDED F5ED"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000 0000"
	$"0000 00FF F5F5 EDED A878 2AF5 F5F5 F5ED"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000 0000"
	$"0000 00FF F5F5 F5ED EDED EDED EDED EDED"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000 0000"
	$"0000 00FF F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000 0000"
	$"0000 00FF F5F5 EDF5 F5F5 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000 0000"
	$"0000 00FF F5F5 F5F5 EDED F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000 0000"
	$"0000 00FF F5F5 EDF5 F5F5 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000 0000"
	$"0000 00FF F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000 0000"
	$"0000 00FF F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000 0000"
	$"0000 00FF F5F5 F5F5 EDF5 F5F5 EDED F5F5"
	$"F5ED F5F5 F5ED EDF5 F5F5 F5FF 0000 0000"
	$"0000 00FF F5F5 F5ED EDF5 F5ED F5F5 EDF5"
	$"EDED F5F5 EDF5 F5ED F5F5 F5FF 0000 0000"
	$"0000 00FF F5F5 F5F5 EDF5 F5ED F5F5 EDF5"
	$"F5ED F5F5 EDF5 F5ED F5F5 F5FF 0000 0000"
	$"0000 00FF F5F5 F5F5 EDF5 F5F5 EDED F5F5"
	$"F5ED F5F5 F5ED EDF5 F5F5 F5FF 0000 0000"
	$"0000 00FF F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000 0000"
	$"0000 00FF F5F5 F5F5 EDF5 F5F5 F5ED F5F5"
	$"F5ED EDF5 F5F5 ED00 F5F5 F5FF 0000 0000"
	$"0000 00FF F5F5 F5ED EDF5 F5F5 EDED F5F5"
	$"EDF5 F5ED F5ED ED00 F5F5 F5FF 0000 0000"
	$"0000 00FF F5F5 F5F5 EDF5 F5F5 F5ED F5F5"
	$"EDF5 F5ED F5F5 ED00 F5F5 F5FF 0000 0000"
	$"0000 00FF F5F5 F5F5 EDF5 F5F5 F5ED F5F5"
	$"F5ED EDF5 F5F5 ED00 F5F5 F5FF 0000 0000"
	$"0000 00FF F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000 0000"
	$"0000 00FF F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000 0000"
	$"0000 00FF F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000 0000"
	$"0000 00FF FFFF FFFF FFFF FFFF FFFF FFFF"
	$"FFFF FFFF FFFF FFFF FFFF FFFF"
};

resource 'ics8' (128) {
	$"EDED EDED EDED EDED EDED EDED ED00 0000"
	$"EDF5 F5F5 F5F5 F52A 78A8 A8A8 EDED 0000"
	$"EDF5 F5F5 F5F5 F5F5 2A78 A8A8 A8ED ED00"
	$"EDF5 F5ED EDED EDED EDED EDED EDED EDED"
	$"EDF5 F5ED EDF5 F5F5 F5F5 F5F5 F5F5 F5ED"
	$"EDF5 F5ED 2AED F5F5 F5F5 F5F5 F5F5 F5ED"
	$"EDF5 F5ED 782A EDED EDED EDED EDF5 F5ED"
	$"EDF5 F5ED A8A8 ED00 00ED D2EB EDF5 F5ED"
	$"EDF5 F5ED EBEB ED00 00ED 78A8 EDF5 F5ED"
	$"EDF5 F5ED EDED EDED EDED 2A54 EDF5 F5ED"
	$"EDF5 F5F5 F5F5 F5F5 F5F5 ED2A EDF5 F5ED"
	$"EDF5 F5F5 F5F5 F5F5 F5F5 F5ED EDF5 F5ED"
	$"EDED EDED EDED EDED EDED EDED EDF5 F5ED"
	$"00ED EDEB D2A8 782A F5F5 F5F5 F5F5 F5ED"
	$"0000 EDED EBD2 A878 2AF5 F5F5 F5F5 F5ED"
	$"0000 00ED EDED EDED EDED EDED EDED EDED"
};


resource 'ics8' (129) {
	$"00FF FFFF FFFF FFFF FFFF FF00 0000 0000"
	$"00FF F5F5 F5F5 F5F5 F5F5 FFFF 0000 0000"
	$"00FF F5ED EDED EDF5 F5F5 FF2B FF00 0000"
	$"00FF F5ED 00ED 00EC F5F5 FFFF FFFF 0000"
	$"00FF F5ED ED00 ECEC F5F5 F5F5 F5FF 0000"
	$"00FF F5ED 00EC 00EC F5F5 F5F5 F5FF 0000"
	$"00FF F5F5 ECEC ECEC F5F5 F5F5 F5FF 0000"
	$"00FF F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000"
	$"00FF F5ED F5F5 F5F5 F5F5 F5F5 F5FF 0000"
	$"00FF F5F5 F5ED EDF5 F5F5 F5F5 F5FF 0000"
	$"00FF F5ED F5F5 F5F5 F5F5 F5F5 F5FF 0000"
	$"00FF F5F5 F5F5 EDF5 EDED EDF5 F5FF 0000"
	$"00FF F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000"
	$"00FF F5F5 F5F5 EDED F5ED F5F5 F5FF 0000"
	$"00FF F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000"
	$"00FF FFFF FFFF FFFF FFFF FFFF FFFF"
};

resource 'ics8' (130) {
	$"00FF FFFF FFFF FFFF FFFF FF00 0000 0000"
	$"00FF F5F5 F5F5 F5F5 F5F5 FFFF 0000 0000"
	$"00FF F5ED EDED EDF5 F5F5 FF2B FF00 0000"
	$"00FF F5ED F5ED F5ED F5F5 FFFF FFFF 0000"
	$"00FF F5ED EDF5 EDED F5F5 F5F5 F5FF 0000"
	$"00FF F5ED F5ED F5ED F5F5 F5F5 F5FF 0000"
	$"00FF F5F5 EDED EDED F5F5 F5F5 F5FF 0000"
	$"00FF F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000"
	$"00FF F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000"
	$"00FF F5F5 F5ED F5F5 EDED F5F5 F5FF 0000"
	$"00FF F5F5 EDED F5ED F5F5 EDF5 F5FF 0000"
	$"00FF F5F5 F5ED F5ED F5F5 EDF5 F5FF 0000"
	$"00FF F5F5 F5ED F5F5 EDED F5F5 F5FF 0000"
	$"00FF F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000"
	$"00FF F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000"
	$"00FF FFFF FFFF FFFF FFFF FFFF FFFF"
};

resource 'icl4' (128) {
	$"6666 6666 6666 6666 6666 6666 6000 0000"
	$"6C0C 0C0C 0C0C 0CCC 7766 6666 6600 0000"
	$"60C0 C0C0 C0C0 C0CC C776 6666 6660 0000"
	$"6C0C 0C0C 0C0C 0C0C CC77 6666 6666 0000"
	$"60C0 C0C0 C0C0 C0C0 CCC7 7666 6666 6000"
	$"6C0C 0C0C 0C0C 0C0C 0CCC 7766 6666 6600"
	$"60C0 C066 6666 6666 6666 6666 6666 6660"
	$"6C0C 0C66 0C0C 0C0C 0C0C 0C0C 0C0C 0C60"
	$"60C0 C060 60C0 C0C0 C0C0 C0C0 C0C0 C060"
	$"6C0C 0C6C 060C 0C0C 0C0C 0C0C 0C0C 0C60"
	$"60C0 C06C C060 C0C0 C0C0 C0C0 C0C0 C060"
	$"6C0C 0C6C CC06 0C0C 0C0C 0C0C 0C0C 0C60"
	$"60C0 C06C CCC0 6666 6666 6666 60C0 C060"
	$"6C0C 0C67 7CCC 6000 0066 6666 6C0C 0C60"
	$"60C0 C066 777C 6000 0066 6666 60C0 C060"
	$"6C0C 0C66 6667 6000 0067 6666 6C0C 0C60"
	$"60C0 C066 6666 6000 006C 7776 60C0 C060"
	$"6C0C 0C66 6666 6000 006C CC77 6C0C 0C60"
	$"60C0 C066 6666 6666 6660 CCCC 60C0 C060"
	$"6C0C 0C0C 0C0C 0C0C 0C06 0CCC 6C0C 0C60"
	$"60C0 C0C0 C0C0 C0C0 C0C0 60CC 60C0 C060"
	$"6C0C 0C0C 0C0C 0C0C 0C0C 060C 6C0C 0C60"
	$"60C0 C0C0 C0C0 C0C0 C0C0 C060 60C0 C060"
	$"6C0C 0C0C 0C0C 0C0C 0C0C 0C06 6C0C 0C60"
	$"6666 6666 6666 6666 6666 6666 60C0 C060"
	$"0666 6666 677C CC0C 0C0C 0C0C 0C0C 0C60"
	$"0066 6666 6677 CCC0 C0C0 C0C0 C0C0 C060"
	$"0006 6666 6667 7CCC 0C0C 0C0C 0C0C 0C60"
	$"0000 6666 6666 77CC C0C0 C0C0 C0C0 C060"
	$"0000 0666 6666 677C CC0C 0C0C 0C0C 0C60"
	$"0000 0066 6666 6666 6666 6666 6666 6660"
};

resource 'icl4' (129) {
	$"000F FFFF FFFF FFFF FFFF FF00 0000 0000"
	$"000F 0C0C 0C0C 0C0C 0C0C 0FF0 0000 0000"
	$"000F C666 6666 66C0 C0C0 CFCF 0000 0000"
	$"000F 0600 0007 666C 0C0C 0FCC F000 0000"
	$"000F C606 6666 6666 C0C0 CFCC CF00 0000"
	$"000F 0606 6000 0006 0C0C 0FCC CCF0 0000"
	$"000F C606 0666 6606 C0C0 CFFF FFFF 0000"
	$"000F 0606 6606 6606 0C0C 0C0C 0C0F 0000"
	$"000F C606 6666 0606 C0C0 C0C0 C0CF 0000"
	$"000F 0600 0000 6606 0C0C 0C0C 0C0F 0000"
	$"000F C666 6666 6606 C0C0 C0C0 C0CF 0000"
	$"000F 0C66 67C0 0006 0C0C 0C0C 0C0F 0000"
	$"000F C0C6 6666 6666 C0C0 C0C0 C0CF 0000"
	$"000F 0C0C 0C0C 0C0C 0C0C 0C0C 0C0F 0000"
	$"000F C060 C0C0 C0C0 C0C0 C0C0 C0CF 0000"
	$"000F 0C0C 660C 0C0C 0C0C 0C0C 0C0F 0000"
	$"000F C060 C0C0 C0C0 C0C0 C0C0 C0CF 0000"
	$"000F 0C0C 0C0C 0C0C 0C0C 0C0C 0C0F 0000"
	$"000F C0C0 C0C0 C0C0 C0C0 C0C0 C0CF 0000"
	$"000F 0C66 6C66 0606 6C66 666C 0C0F 0000"
	$"000F C0C0 C0C0 C0C0 C0C0 C0C0 C0CF 0000"
	$"000F 0C66 0666 066C 666C 6C0C 0C0F 0000"
	$"000F C0C0 C0C0 C0C0 C0C0 C0C0 C0CF 0000"
	$"000F 0C66 6606 6C66 0606 6C0C 0C0F 0000"
	$"000F C0C0 C0C0 C0C0 C0C0 C0C0 C0CF 0000"
	$"000F 0C66 066C 6666 0666 0C0C 0C0F 0000"
	$"000F C0C0 C0C0 C0C0 C0C0 C0C0 C0CF 0000"
	$"000F 0C66 6606 660C 6666 060C 0C0F 0000"
	$"000F C0C0 C0C0 C0C0 C0C0 C0C0 C0CF 0000"
	$"000F 0C0C 0C0C 0C0C 0C0C 0C0C 0C0F 0000"
	$"000F C0C0 C0C0 C0C0 C0C0 C0C0 C0CF 0000"
	$"000F FFFF FFFF FFFF FFFF FFFF FFFF"
};

resource 'icl4' (130) {
	$"000F FFFF FFFF FFFF FFFF FF00 0000 0000"
	$"000F 0C0C 0C0C 0C0C 0C0C 0FF0 0000 0000"
	$"000F C666 6666 66C0 C0C0 CFCF 0000 0000"
	$"000F 0600 0007 666C 0C0C 0FCC F000 0000"
	$"000F C606 6666 6666 C0C0 CFCC CF00 0000"
	$"000F 0606 6000 0006 0C0C 0FCC CCF0 0000"
	$"000F C606 0666 6606 C0C0 CFFF FFFF 0000"
	$"000F 0606 6606 6606 0C0C 0C0C 0C0F 0000"
	$"000F C606 6666 0606 C0C0 C0C0 C0CF 0000"
	$"000F 0600 0000 6606 0C0C 0C0C 0C0F 0000"
	$"000F C666 6666 6606 C0C0 C0C0 C0CF 0000"
	$"000F 0C66 6700 0006 0C0C 0C0C 0C0F 0000"
	$"000F C0C6 6666 6666 C0C0 C0C0 C0CF 0000"
	$"000F 0C0C 0C0C 0C0C 0C0C 0C0C 0C0F 0000"
	$"000F C060 C0C0 C0C0 C0C0 C0C0 C0CF 0000"
	$"000F 0C0C 660C 0C0C 0C0C 0C0C 0C0F 0000"
	$"000F C060 C0C0 C0C0 C0C0 C0C0 C0CF 0000"
	$"000F 0C0C 0C0C 0C0C 0C0C 0C0C 0C0F 0000"
	$"000F C0C0 C0C0 C0C0 C0C0 C0C0 C0CF 0000"
	$"000F 0C0C 6C0C 660C 060C 066C 0C0F 0000"
	$"000F C0C6 60C6 C060 66C0 60C6 C0CF 0000"
	$"000F 0C0C 6C06 0C6C 060C 6C06 0C0F 0000"
	$"000F C0C0 60C0 66C0 C6C0 C660 C0CF 0000"
	$"000F 0C0C 0C0C 0C0C 0C0C 0C0C 0C0F 0000"
	$"000F C0C0 60C0 C6C0 C660 C060 C0CF 0000"
	$"000F 0C06 6C0C 660C 6C06 066C 0C0F 0000"
	$"000F C0C0 60C0 C6C0 60C6 C060 C0CF 0000"
	$"000F 0C0C 6C0C 060C 066C 0C6C 0C0F 0000"
	$"000F C0C0 C0C0 C0C0 C0C0 C0C0 C0CF 0000"
	$"000F 0C0C 0C0C 0C0C 0C0C 0C0C 0C0F 0000"
	$"000F C0C0 C0C0 C0C0 C0C0 C0C0 C0CF 0000"
	$"000F FFFF FFFF FFFF FFFF FFFF FFFF"
};

resource 'ics4' (128) {
	$"6666 6666 6666 6000 6C0C 0C0C 7666 6600"
	$"60C0 C0C0 C766 6660 6C06 6666 6666 6666"
	$"60C6 60C0 C0C0 C0C6 6C06 C60C 0C0C 0C06"
	$"60C6 7C66 6666 60C6 6C06 6660 0666 6C06"
	$"60C6 6660 0676 60C6 6C06 6666 66CC 6C06"
	$"60C0 C0C0 C06C 60C6 6C0C 0C0C 0C06 6C06"
	$"6666 6666 6666 60C6 0666 667C 0C0C 0C06"
	$"0066 6667 C0C0 C0C6 0006 6666 6666 6666"
};

resource 'ics4' (129) {
	$"0FFF FFFF FFF0 0000 0F0C 0C0C 0CFF 0000"
	$"0FC6 6660 C0FC F000 0F06 0606 0CFF FF00"
	$"0FC6 6066 C0C0 CF00 0F06 0606 0C0C 0F00"
	$"0FC0 6666 C0C0 CF00 0F0C 0C0C 0C0C 0F00"
	$"0FC6 C0C0 C0C0 CF00 0F0C 066C 0C0C 0F00"
	$"0FC6 C0C0 C0C0 CF00 0F0C 0C6C 666C 0F00"
	$"0FC0 C0C0 C0C0 CF00 0F0C 0C66 060C 0F00"
	$"0FC0 C0C0 C0C0 CF00 0FFF FFFF FFFF FF"
};

resource 'ics4' (130) {
	$"0FFF FFFF FFF0 0000 0F0C 0C0C 0CFF 0000"
	$"0FC6 6660 C0FC F000 0F06 0606 0CFF FF00"
	$"0FC6 6066 C0C0 CF00 0F06 0606 0C0C 0F00"
	$"0FC0 6666 C0C0 CF00 0F0C 0C0C 0C0C 0F00"
	$"0FC0 C0C0 C0C0 CF00 0F0C 060C 660C 0F00"
	$"0FC0 66C6 C060 CF00 0F0C 0606 0C6C 0F00"
	$"0FC0 C6C0 66C0 CF00 0F0C 0C0C 0C0C 0F00"
	$"0FC0 C0C0 C0C0 CF00 0FFF FFFF FFFF FF"
};

