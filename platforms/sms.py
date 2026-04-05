# Platform implementation: sms (Python)
# Implements the functions declared in sms.zero.md
# Uses Vonage SMS API. Credentials from env vars or ../fieldnote/.env

import os


def _load_vonage_credentials():
    """Load Vonage API credentials from environment or .env file."""
    key = os.environ.get("VONAGE_API_KEY")
    secret = os.environ.get("VONAGE_API_SECRET")
    if key and secret:
        return key, secret
    # try loading from .env — check script directory and parent directories
    script_dir = os.path.dirname(os.path.abspath(__file__))
    for d in [script_dir, os.path.dirname(script_dir), os.path.dirname(os.path.dirname(script_dir))]:
        env_path = os.path.join(d, "platforms", ".env")
        if os.path.exists(env_path):
            break
        env_path = os.path.join(d, ".env")
        if os.path.exists(env_path):
            break
    else:
        env_path = None
    if env_path:
        for line in open(env_path):
            line = line.strip()
            if line.startswith("VONAGE_API_KEY="):
                key = line.split("=", 1)[1]
            elif line.startswith("VONAGE_API_SECRET="):
                secret = line.split("=", 1)[1]
    return key, secret


# @zero on send sms (string message) to (string phone)
def fn_send_sms__string_to__string(message: str, phone: str):
    import urllib.request
    import urllib.parse
    import json
    key, secret = _load_vonage_credentials()
    if not key or not secret:
        print(f"sms: no credentials, would send to {phone}: {message}")
        return
    # strip + prefix for Vonage
    to = phone.lstrip("+")
    data = json.dumps({
        "from": "noob",
        "text": message,
        "to": phone,
        "api_key": key,
        "api_secret": secret,
    }).encode("utf-8")
    req = urllib.request.Request(
        "https://rest.nexmo.com/sms/json",
        data=data,
        headers={"Content-Type": "application/json"},
    )
    try:
        resp = urllib.request.urlopen(req, timeout=10)
        result = json.loads(resp.read().decode())
        status = result.get("messages", [{}])[0].get("status", "?")
        if status == "0":
            print(f"sms: sent to {to}")
        else:
            print(f"sms: failed to {to}: {result}")
    except Exception as e:
        print(f"sms: error sending to {to}: {e}")
