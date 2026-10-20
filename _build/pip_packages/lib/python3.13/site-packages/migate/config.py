from rich.console import Console
from rich.theme import Theme

BASE_URL              = "https://account.xiaomi.com"
SERVICELOGIN_URL      = BASE_URL + "/pass/serviceLogin"
SERVICELOGINAUTH2_URL = SERVICELOGIN_URL + "Auth2"
LIST_URL              = BASE_URL + "/identity/list"
SEND_EM_TICKET        = BASE_URL + "/identity/auth/sendEmailTicket"
SEND_PH_TICKET        = BASE_URL + "/identity/auth/sendPhoneTicket"
VERIFY_EM             = BASE_URL + "/identity/auth/verifyEmail"
VERIFY_PH             = BASE_URL + "/identity/auth/verifyPhone"
USERQUOTA_URL         = BASE_URL + "/identity/pass/sms/userQuota"

c_theme = Theme({
    "orange": "bold #ff6900",
    "green":  "bold #00c853",
    "red":    "bold #d50000",
    "white":  "bold white",
})
console = Console(theme=c_theme)

loader = console.status("", spinner="dots")
