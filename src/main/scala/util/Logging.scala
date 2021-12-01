package util

import org.slf4j.LoggerFactory

trait WithLogger {
    lazy private val logger = LoggerFactory.getLogger(loggerName)
    private def loggerName = getClass.getName

    def logInfo(msg: String): Unit = logger.info(msg)
}
