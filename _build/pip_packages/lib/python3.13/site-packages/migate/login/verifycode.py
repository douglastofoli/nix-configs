import json

from migate.config import VERIFY_EM, VERIFY_PH, console
from migate.requester import post


def verify_code_ticket(addressType, label):
    url = VERIFY_EM if addressType == "EM" else VERIFY_PH

    while True:
        console.print(f"[white]Check your {label} for the code.[/]")
        ticket = console.input("[orange]Enter code (or type 'r' to resend): [/]").strip()

        if ticket == "r":
            return "RESEND"

        try:
            response      = post(url, data={"ticket": ticket, "trust": "true", "_json": "true"})
            response_text = json.loads(response.text[11:])
        except Exception as e:
            return {"error": f"Connection error: {str(e)}"}

        if response_text.get("code") == 0:
            return response_text.get("location")

        if response_text.get("code") == 70014:
            console.print("Invalid code provided.", style="red")
            continue

        return {"error": response_text}
