import logging

logging.basicConfig(format='[%(levelname)s][%(asctime)s] %(message)s', level=logging.INFO)

LOG_DEBUG = logging.debug
LOG_INFO = logging.info
LOG_WARNING = logging.warning
LOG_ERROR = logging.error
LOG_CRITICAL = logging.critical