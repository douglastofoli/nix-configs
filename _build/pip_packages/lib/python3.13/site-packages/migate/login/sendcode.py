import json

from migate.login.captcha import handle_captcha
from migate.config import SEND_EM_TICKET, SEND_PH_TICKET, console
from migate.requester import post

def send_verification_code(addressType, label):
    if addressType == "EM":
        send_url = SEND_EM_TICKET
    else:
        send_url = SEND_PH_TICKET

    try:
        response      = post(send_url)
        response_text = json.loads(response.text[11:])
    except Exception as e:
        return {"error": f"Connection error: {str(e)}"}

    if response_text.get("code") == 87001:
        console.print("\nCAPTCHA verification required for sending code!\n", style="orange")
        response = handle_captcha(send_url, response, {"icode": "", "_json": "true"}, "icode")

        if isinstance(response, dict) and "error" in response:
            return response

        response_text = json.loads(response.text[11:])

    if response_text.get("code") == 0:
        console.print(f"\nCode sent to {label} successfully.\n", style="green")
        return {"success": True}

    code      = response_text.get("code")
    error_msg = response_text.get("tips", response_text) if code == 70022 else response_text
    return {"error": error_msg}
