// Platform implementation: sms (TypeScript)
// Implements the functions declared in sms.zero.md
// Server-side only — not used in client bundle

// @zero on send sms (string message) to (string phone)
function fn_send_sms__string_to__string(message: string, phone: string): void {
    console.log(`sms: would send to ${phone}: ${message}`);
}
