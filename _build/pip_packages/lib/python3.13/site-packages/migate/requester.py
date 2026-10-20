import time
import requests
from migate.config import loader

TIMEOUT = 15
RETRIES = 3
BACKOFF = 2

session = requests.Session()
session.headers.update({
    "User-Agent":      "offici5l/migate",
    "Content-Type":    "application/x-www-form-urlencoded",
    "Accept":          "application/json;charset=UTF-8",
    "Accept-Language": "en-US,en;q=0.9"
})


def get(url, **kwargs):
    return _request("GET", url, **kwargs)


def post(url, **kwargs):
    return _request("POST", url, **kwargs)


def _request(method, url, **kwargs):
    kwargs.setdefault("timeout", TIMEOUT)
    last_error = None

    for attempt in range(RETRIES):
        try:
            loader.start()
            response = session.request(method, url, **kwargs)
            response.raise_for_status()
            return response
        except Exception as e:
            last_error = e
            if attempt < RETRIES - 1:
                time.sleep(BACKOFF ** (attempt + 1))
        finally:
            loader.stop() 

    raise ConnectionError(f"Failed after {RETRIES} attempts: {last_error}")
