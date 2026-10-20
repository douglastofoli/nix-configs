import json
import time
from migate.login.sendcode import send_verification_code
from migate.login.verifycode import verify_code_ticket
from migate.config import LIST_URL, SERVICELOGINAUTH2_URL, USERQUOTA_URL, console
from migate.requester import get, post


def handle_verify(context, auth_data):
    console.print("\n=== 2FA Verification Required ===\n", style="orange")

    try:
        response    = get(LIST_URL, params={"sid": auth_data["sid"], "supportedMask": "0", "context": context})
        result_json = json.loads(response.text[11:])
    except Exception as e:
        return {"error": f"Connection error: {str(e)}"}

    options = result_json.get("options", [])

    if 8 in options and 4 in options:
        console.print("Choose verification method:", style="white")
        console.print("[orange]1[/][white] = Phone (SMS)[/]")
        console.print("[orange]2[/][white] = Email[/]")
        choice = console.input("[white]Enter 1 or 2: [/]").strip()

        if choice not in ["1", "2"]:
            return {"error": "Invalid choice!"}
        addressType = "PH" if choice == "1" else "EM"
    elif 4 in options:
        addressType = "PH"
    elif 8 in options:
        addressType = "EM"
    else:
        return {"error": f"No supported verification options found. (Response: {result_json})"}

    label = "Email" if addressType == "EM" else "Phone"

    while True:
        try:
            response_quota = post(USERQUOTA_URL, data={"addressType": addressType, "contentType": "160040", "_json": "true"})
            quota_json     = json.loads(response_quota.text[11:])
        except Exception as e:
            return {"error": f"Connection error during quota check: {str(e)}"}

        info      = quota_json.get("info")
        remaining = int(info) if info is not None else 0
        console.print(f"\n[white]Attempts remaining: [/][{'green' if remaining > 0 else 'red'}]{remaining}[/]")

        if remaining == 0:
            return {"error": f"Sent too many codes to {label}. Try again tomorrow."}

        send_result = send_verification_code(addressType, label)

        if isinstance(send_result, dict) and "error" in send_result:
            err_data = send_result["error"]
            if isinstance(err_data, dict) and err_data.get("code") == 20024:
                wt_seconds = err_data.get("data", {}).get("wt", 60)
                for i in range(int(wt_seconds), 0, -1):
                    print(f"\rPlease wait: {i} before you can try resend again", end="", flush=True)
                    time.sleep(1)
                console.input("\n[white]Press Enter to try resending now... [/]")
                continue

            return send_result

        verify_result = verify_code_ticket(addressType, label)

        if verify_result == "RESEND":
            console.print("\n[orange]Retrying to send the code...[/]\n")
            continue

        if isinstance(verify_result, dict) and "error" in verify_result:
            return verify_result

        break

    try:
        response = get(verify_result, allow_redirects=False)
        location = response.headers.get("Location")
        get(location, allow_redirects=False)
        return post(SERVICELOGINAUTH2_URL, data=auth_data)
    except Exception as e:
        return {"error": f"Connection error: {str(e)}"}
