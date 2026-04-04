// Platform implementation: sms (TypeScript)
// Implements the functions declared in sms.zero.md
// Server-side only — not used in client bundle

// @zero on send sms (string to) (string message)
function fn_send_sms__string__string(to: string, message: string): void {
    console.log(`sms: would send to ${to}: ${message}`);
}
