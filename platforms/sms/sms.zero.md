# sms
*send SMS messages*

@server

## specification

Send SMS messages via an external provider (Vonage). Credentials are read from environment variables VONAGE_API_KEY and VONAGE_API_SECRET.

## interface

Send an SMS message to a phone number:

    on send sms (string message) to (string phone)
