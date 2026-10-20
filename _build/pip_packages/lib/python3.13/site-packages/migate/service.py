import json
import base64
import hashlib
from urllib.parse import quote

from migate.config import SERVICELOGIN_URL, console
from migate.requester import session, get


def get_service(auth_cookies, params):
    params["_json"] = True

    deviceId = auth_cookies["deviceId"]

    for k, v in auth_cookies.items():
        session.cookies.set(k, v)

    try:
        response      = get(SERVICELOGIN_URL, params=params)
        response_text = json.loads(response.text[11:])
    except Exception as e:
        console.print(f"\n[red]Connection error: {e}[/]\n")
        raise SystemExit(1)

    nonce     = response_text.get("nonce")
    ssecurity = response_text.get("ssecurity")
    location  = response_text.get("location")
    cUserId   = response_text.get("cUserId")
    psecurity = response_text.get("psecurity")

    if not nonce or not ssecurity:
        console.print(f"\n[red]Missing service data | Response: {response_text}[/]\n")
        raise SystemExit(1)

    client_sign = quote(base64.b64encode(
        hashlib.sha1(f"nonce={nonce}&{ssecurity}".encode()).digest()
    ))

    try:
        response        = get(f"{location}&clientSign={client_sign}")
        service_cookies = response.cookies.get_dict()
    except Exception as e:
        console.print(f"\n[red]Connection error: {e}[/]\n")
        raise SystemExit(1)

    session.cookies.clear()

    return {
        "servicedata": {
            "nonce":     nonce,
            "ssecurity": ssecurity,
            "cUserId":   cUserId,
            "psecurity": psecurity,
            "deviceId":  deviceId,
        },
        "cookies": service_cookies,
    }
