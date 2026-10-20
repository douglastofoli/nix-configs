import getpass
import hashlib
import json
import uuid
from urllib.parse import urlparse, parse_qs
from pathlib import Path

from migate.login.captcha import handle_captcha
from migate.login.verify import handle_verify
from migate.config import SERVICELOGINAUTH2_URL, SERVICELOGIN_URL, console
from migate.requester import session, get, post

def _get_device_id(user: str) -> str:
    id_file = Path.home() / f".migate_device_{hashlib.md5(user.encode()).hexdigest()[:8]}.json"
    if id_file.exists():
        return json.loads(id_file.read_text())["deviceId"]
    deviceId = "wb_" + uuid.uuid4().hex
    id_file.write_text(json.dumps({"deviceId": deviceId}))
    return deviceId


def get_passtoken(auth_data):

    sid          = auth_data["sid"]
    cookies_file = Path.home() / f".{sid}" / "cookies.json"

    if cookies_file.exists():
        try:
            with open(cookies_file, "r") as f:
                passToken = json.load(f)
        except (json.JSONDecodeError, KeyError):
            cookies_file.unlink()
            console.print("Session corrupted, please log in again.\n", style="red")
            passToken = None

        if passToken is not None:
            choice = console.input(
                f"[green]Already logged in[/]\n"
                f"[white]Account ID: [/][orange]{passToken['userId']}[/]\n\n"
                f"[white]Press 'Enter' to continue[/]\n"
                f"[white](To log out, type [red]2[/] and press Enter.): "
            ).strip().lower()

            if choice == "2":
                cookies_file.unlink()
                console.print("Logged out.", style="red")
            else:
                return passToken

    auth_data["_json"] = True

    try:
        response      = get(SERVICELOGIN_URL, params=auth_data)
        response_text = json.loads(response.text[11:])
    except Exception as e:
        console.print(f"\n[red]Connection error: {e}[/]\n")
        raise SystemExit(1)

    auth_data["serviceParam"] = response_text["serviceParam"]
    auth_data["qs"]           = response_text["qs"]
    auth_data["callback"]     = response_text["callback"]
    auth_data["_sign"]        = response_text["_sign"]

    while True:
        user      = console.input("[white]Account ID / Email / Phone (+): [/]").strip()
        pwd_input = getpass.getpass("Password: ").strip()
        pwd       = hashlib.md5(pwd_input.encode()).hexdigest().upper()

        auth_data["user"] = user
        auth_data["hash"] = pwd

        session.cookies.set("deviceId", _get_device_id(user))

        try:
            response      = post(SERVICELOGINAUTH2_URL, data=auth_data)
            response_text = json.loads(response.text[11:])
        except Exception as e:
            console.print(f"\n[red]Connection error: {e}[/]\n")
            raise SystemExit(1)

        if response_text.get("code") == 70016:
            console.print("\nInvalid password or username! Please try again.\n", style="red")
            continue

        if response_text.get("code") == 87001:
            console.print("\nCAPTCHA verification required!\n", style="orange")
            response = handle_captcha(SERVICELOGINAUTH2_URL, response, auth_data, "captCode")

            if isinstance(response, dict) and "error" in response:
                console.print(f"\n[red]{response['error']}[/]\n")
                raise SystemExit(1)

            response_text = json.loads(response.text[11:])

            if response_text.get("code") == 70016:
                console.print("\nInvalid password or username! Please try again.\n", style="red")
                continue

        break

    if "notificationUrl" in response_text:
        notification_url = response_text["notificationUrl"]
        if any(x in notification_url for x in ["callback", "SetEmail", "BindAppealOrSafePhone"]):
            console.print(f"\n[red]Action required at: {notification_url}[/]\n")
            raise SystemExit(1)

        context  = parse_qs(urlparse(notification_url).query)["context"][0]
        response = handle_verify(context, auth_data)

        if isinstance(response, dict) and "error" in response:
            console.print(f"\n[red]{response['error']}[/]\n")
            raise SystemExit(1)

        response_text = json.loads(response.text[11:])

    cookies  = session.cookies.get_dict()
    required = {"deviceId", "passToken", "userId"}
    missing  = required - cookies.keys()
    if missing:
        console.print(f"\n[red]Missing keys: {', '.join(missing)} | Response: {response_text}[/]\n")
        raise SystemExit(1)

    passToken = {k: cookies[k] for k in required}

    cookies_file.parent.mkdir(parents=True, exist_ok=True)
    with open(cookies_file, "w") as f:
        json.dump(passToken, f)

    console.print("\nLogin successful\n", style="green")

    session.cookies.clear()
    
    return passToken
