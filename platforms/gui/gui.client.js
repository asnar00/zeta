// Platform implementation: gui (browser/client)
// Implements the functions declared in gui.zero.md
// These run in the browser, not on the server.

// @zero on (string result$) <- input (string prompt)
async function task_input__string(prompt) {
    return new Promise((resolve) => {
        const container = document.createElement("div");
        container.style.cssText = "text-align:center; margin-top:20px;";
        const label = document.createElement("div");
        label.textContent = prompt;
        label.style.cssText = "color:#1a1a1a; font-family:'Switzer',sans-serif; font-size:14pt; margin-bottom:8px;";
        const input = document.createElement("input");
        input.type = "text";
        input.style.cssText = "font-family:'Switzer',sans-serif; font-size:14pt; padding:8px; text-align:center; border:1px solid #1a1a1a; outline:none; background:transparent;";
        container.appendChild(label);
        container.appendChild(input);
        document.body.appendChild(container);
        input.focus();
        input.addEventListener("keydown", (e) => {
            if (e.key === "Enter") {
                const value = input.value;
                container.remove();
                resolve(value);
            }
        });
    });
}

// @zero on show message (string text)
function fn_show_message__string(text) {
    alert(text);
}

// @zero input string cookie$[string]
const cookie_arr = new Proxy({}, {
    get(_, name) {
        const match = document.cookie.match(new RegExp("(?:^|; )" + name + "=([^;]*)"));
        return match ? match[1] : "";
    }
});

// @zero on clear cookie (string name)
function fn_clear_cookie__string(name) {
    const secure = location.protocol === "https:" ? "; Secure" : "";
    document.cookie = name + "=; path=/; expires=Thu, 01 Jan 1970 00:00:00 GMT; SameSite=Lax" + secure;
}

// @zero on (string choice$) <- choose (string option-a) or (string option-b)
async function task_choose_or__string__string(optionA, optionB) {
    return new Promise((resolve) => {
        const container = document.createElement("div");
        container.style.cssText = "text-align:center; margin-top:20px;";
        const makeBtn = (label) => {
            const btn = document.createElement("button");
            btn.textContent = label;
            btn.style.cssText = "font-family:'Switzer',sans-serif; font-size:14pt; padding:8px 24px; margin:0 8px; cursor:pointer; border:1px solid #1a1a1a; background:transparent;";
            btn.addEventListener("click", () => { container.remove(); resolve(label); });
            return btn;
        };
        container.appendChild(makeBtn(optionA));
        container.appendChild(makeBtn(optionB));
        document.body.appendChild(container);
    });
}

// @zero on set cookie of (string name) to (string value)
function fn_set_cookie_of__string_to__string(name, value) {
    const secure = location.protocol === "https:" ? "; Secure" : "";
    document.cookie = name + "=" + value + "; path=/; SameSite=Lax" + secure;
}

// @zero on reload page ()
function fn_reload_page() {
    location.reload();
}

// @zero on click on (string selector)
async function fn_click_on__string(selector) {
    const el = document.querySelector(selector);
    if (!el) return "error: element not found: " + selector;
    el.click();
    await new Promise(r => setTimeout(r, 100));
    return "ok";
}

// @zero on type (string text) into input box (string selector)
function fn_type__string_into_input_box__string(text, selector) {
    const el = document.querySelector(selector);
    if (!el) return "error: element not found: " + selector;
    el.focus();
    el.value = text;
    el.dispatchEvent(new Event("input", { bubbles: true }));
    return "ok";
}

// @zero on press (string key) on (string selector)
function fn_press__string_on__string(key, selector) {
    const el = document.querySelector(selector);
    if (!el) return "error: element not found: " + selector;
    el.dispatchEvent(new KeyboardEvent("keydown", { key: key, bubbles: true }));
    return "ok";
}

// @zero on (string snapshot) = describe page ()
function fn_describe_page() {
    const lines = [];
    const vw = window.innerWidth, vh = window.innerHeight;
    lines.push("viewport " + vw + "x" + vh);

    function describeEl(el, depth) {
        const rect = el.getBoundingClientRect();
        const cs = window.getComputedStyle(el);
        if (cs.display === "none" || rect.width === 0 && rect.height === 0) return;

        const indent = "  ".repeat(depth);
        let tag = el.tagName.toLowerCase();
        if (el.className && typeof el.className === "string") tag += "." + el.className.trim().replace(/\s+/g, ".");
        if (el.id) tag += "#" + el.id;

        const x = Math.round(rect.left), y = Math.round(rect.top);
        const w = Math.round(rect.width), h = Math.round(rect.height);
        const offscreen = (x + w < 0 || y + h < 0 || x > vw || y > vh);
        const visible = cs.visibility !== "hidden" && cs.opacity !== "0" && !offscreen;

        let line = indent + tag + " (" + x + "," + y + " " + w + "x" + h;
        if (!visible) line += " hidden";
        if (offscreen) line += " offscreen";
        line += ")";

        const bg = cs.backgroundColor;
        if (bg && bg !== "rgba(0, 0, 0, 0)" && bg !== "transparent") line += " bg:" + bg;
        const font = cs.fontFamily.split(",")[0].trim().replace(/['"]/g, "");
        const size = cs.fontSize;
        line += " " + font + " " + size;
        const color = cs.color;
        if (color) line += " color:" + color;
        const zi = cs.zIndex;
        if (zi && zi !== "auto") line += " z:" + zi;

        const text = Array.from(el.childNodes)
            .filter(n => n.nodeType === 3 && n.textContent.trim())
            .map(n => n.textContent.trim())
            .join(" ");
        if (text) line += ' "' + text + '"';
        if (el.tagName === "INPUT") {
            line += ' value="' + (el.value || "") + '"';
            if (document.activeElement === el) line += " focused";
        }
        if (el.tagName === "BUTTON") {
            line += ' "' + (el.textContent || "").trim() + '"';
        }

        lines.push(line);
        for (const child of el.children) {
            describeEl(child, depth + 1);
        }
    }

    describeEl(document.documentElement, 0);
    return lines.join("\n");
}
