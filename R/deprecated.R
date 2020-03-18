#' @title GPS data pipeline
#' @export
BUTEOatEUROPE.pipeline <- function() {

    cat( red$bold('\n ----> Get new emails and extract attachments ......\n') )
    extract_email_attachements(maildir="GSM_MTI")

    cat( blue$bold('\n ----> Update gps table.....\n') )
    a = scidbupdate_mti_gps.BUTEOatEUROPE()
    push_msg(a, 'BUTEOatEUROPE.mti_gps')


    cat( green$bold('\n ----> Update sensors table....\n') )
    b = scidbupdate_mti_sensors.BUTEOatEUROPE()
    push_msg(b, 'BUTEOatEUROPE.mti_sensors')


    }
