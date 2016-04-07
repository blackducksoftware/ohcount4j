#include <cmqc.h>      /* MQ API header file       */
//

#define NUMBEROFSELECTORS  2

const MQHCONN Hconn = MQHC_DEF_HCONN;


static void InquireGetAndPut(char   *Message,
		PMQHOBJ pHobj,
		char   *Object)
{
	if (CompCode != MQCC_OK)
	{
		sprintf(Message, MESSAGE_4_E,
				ERROR_IN_MQINQ, CompCode, Reason);
		SetMsg(Message);
	}
	else
	{
		/* Process the changes */
	} /* end if CompCode */
}