# ᕦ(ツ)ᕤ
# qemu.py
# author: asnaroo
# zero to anything

import struct
import subprocess
import socket
import json
import time
import select
from util import *

#--------------------------------------------------------------------------------------------------
# run qemu; connect with it, dump when it shuts down

def run_qemu(filename) -> bool:
    # Launch QEMU with QMP enabled, paused start, and -no-shutdown.
    qemu_cmd = [
        "qemu-system-riscv32",
        "-machine", "virt",
        "-bios", filename,
        "-nographic",
        "-no-reboot",
        "-no-shutdown",          # Prevent automatic shutdown on guest shutdown.
        "-S",                    # Start QEMU paused.
        "-qmp", "tcp:localhost:4444,server,nowait",  # Enable QMP interface.
        "-serial", "mon:stdio"
    ]
    
    qemu_proc = subprocess.Popen(qemu_cmd)
    
    # Give QEMU a brief moment to start and open the QMP socket
    time.sleep(0.1)
    
    # Connect to the QMP interface.
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.connect(("localhost", 4444))
    
    # Read and print the QMP greeting message.
    greeting = sock.recv(4096).decode('utf-8')
    #print("QMP Greeting:", greeting)
    
    # Enable QMP capabilities.
    qmp_command(sock, {"execute": "qmp_capabilities"})
    
    # Resume QEMU execution.
    qmp_command(sock, {"execute": "cont"})
    
    # Instead of a fixed sleep, wait for the "STOP" event,
    # which indicates QEMU has paused (after guest shutdown).
    result = wait_for_event(sock, "STOP", timeout=5)
    if result is None:
        log("qemu did not stop")
        return False
    
    # Dump memory: 1K from address 0x80000000 to file hello.dump.
    dump_cmd = {
        "execute": "human-monitor-command",
        "arguments": {
            "command-line": f"pmemsave 0x80000000 1024 {filename.replace('.elf', '.dump')}"
        }
    }
    qmp_command(sock, dump_cmd)
    
    # Issue the quit command to terminate QEMU.
    qmp_command(sock, {"execute": "quit"})
    
    # Close the socket and wait for QEMU to exit.
    sock.close()
    qemu_proc.wait()
    return True
#--------------------------------------------------------------------------------------------------
# write elf file

def write_elf(machine: str, filename, code_bytes, rodata_bytes, rwdata_size_bytes):
    if machine != "RISCV32":
        log("not writing elf for " + machine)
        return
    
    # Constants for ELF32
    ELFCLASS32   = 1
    ELFDATA2LSB  = 1
    EV_CURRENT   = 1
    ET_EXEC      = 2
    EM_RISCV     = 243  # RISC-V machine type
    PT_LOAD      = 1
    PF_X         = 1
    PF_W         = 2
    PF_R         = 4
    PAGE_SIZE    = 0x1000

    # Sizes for ELF32 structures
    header_size   = 52  # ELF header size for 32-bit
    ph_entry_size = 32  # Program header entry size for 32-bit
    ph_num        = 3   # Three loadable segments: .text, .rodata, and .bss

    # Calculate offsets.
    ph_table_size = ph_entry_size * ph_num
    # Place the header and program headers at the beginning;
    # ensure the first loadable segment (code) starts at a page boundary.
    first_segment_offset = PAGE_SIZE

    # Compute file offsets.
    code_offset    = first_segment_offset
    code_size      = len(code_bytes)
    
    # Place rodata immediately after code (contiguous layout).
    rodata_offset  = code_offset + code_size
    rodata_size    = len(rodata_bytes)
    
    # Place BSS immediately after rodata.
    rwdata_offset  = rodata_offset + rodata_size

    # Set virtual addresses contiguously, with code loading at 0x80000000.
    code_vaddr   = 0x80000000
    rodata_vaddr = code_vaddr + code_size
    rwdata_vaddr = rodata_vaddr + rodata_size

    # Build the ELF header.
    e_ident = b'\x7fELF' + bytes([ELFCLASS32, ELFDATA2LSB, EV_CURRENT, 0]) + bytes(8)
    e_type    = ET_EXEC
    e_machine = EM_RISCV
    e_version = EV_CURRENT
    e_entry   = code_vaddr  # Entry point is at the beginning of the code segment.
    e_phoff   = header_size  # Program header table follows immediately after ELF header.
    e_shoff   = 0  # No section header table.
    e_flags   = 0
    e_ehsize  = header_size
    e_phentsize = ph_entry_size
    e_phnum   = ph_num
    e_shentsize = 0
    e_shnum   = 0
    e_shstrndx = 0

    elf_header = struct.pack("<16sHHIIIIIHHHHHH",
                             e_ident,
                             e_type,
                             e_machine,
                             e_version,
                             e_entry,
                             e_phoff,
                             e_shoff,
                             e_flags,
                             e_ehsize,
                             e_phentsize,
                             e_phnum,
                             e_shentsize,
                             e_shnum,
                             e_shstrndx)

    # Build program header for the code segment.
    ph_code = struct.pack("<IIIIIIII",
                          PT_LOAD,
                          code_offset,   # file offset
                          code_vaddr,    # virtual address
                          code_vaddr,    # physical address
                          code_size,     # file size
                          code_size,     # memory size
                          PF_R | PF_X,   # permissions: read and execute
                          PAGE_SIZE)     # alignment

    # Build program header for the rodata segment.
    ph_rodata = struct.pack("<IIIIIIII",
                            PT_LOAD,
                            rodata_offset,  # file offset
                            rodata_vaddr,   # virtual address
                            rodata_vaddr,   # physical address
                            rodata_size,    # file size
                            rodata_size,    # memory size
                            PF_R,           # permissions: read-only
                            PAGE_SIZE)

    # Build program header for the BSS segment.
    # Note: file size is zero so that this segment will be zero-initialized.
    ph_rwdata = struct.pack("<IIIIIIII",
                            PT_LOAD,
                            rwdata_offset,      # file offset (points to where data would be)
                            rwdata_vaddr,       # virtual address
                            rwdata_vaddr,       # physical address
                            0,                  # file size 0 (BSS)
                            rwdata_size_bytes,  # memory size (BSS size)
                            PF_R | PF_W,        # permissions: read and write
                            PAGE_SIZE)

    header_data = elf_header + ph_code + ph_rodata + ph_rwdata

    # Build the complete file image.
    file_image = bytearray()
    file_image += header_data

    # Pad from end of header to code_offset.
    pad_len = code_offset - len(file_image)
    file_image += b'\x00' * pad_len

    # Insert the code blob.
    file_image += code_bytes

    # Since we're laying out the segments contiguously, immediately insert rodata.
    file_image += rodata_bytes

    # We do not write any bytes for BSS; it will be zero-initialized at runtime.

    # Write out the final ELF file.
    with open(filename, "wb") as f:
        f.write(file_image)


#--------------------------------------------------------------------------------------------------
# below the line

# send a command to a running qemu instance
def qmp_command(sock, cmd):
    """Send a QMP command and return the response."""
    message = json.dumps(cmd) + "\n"
    sock.sendall(message.encode('utf-8'))
    response = sock.recv(4096).decode('utf-8')
    #print("QMP Response:", response)
    return response

# wait for a qmp event
def wait_for_event(sock, event_name, timeout=5):
    """
    Wait for a QMP event with the given name.
    Returns the event message as a dict or None on timeout.
    """
    start = time.time()
    while time.time() - start < timeout:
        ready = select.select([sock], [], [], 0.5)[0]
        if ready:
            data = sock.recv(4096).decode('utf-8')
            if data:
                # QMP messages can be concatenated, so split by newline.
                for line in data.strip().splitlines():
                    try:
                        msg = json.loads(line)
                        if "event" in msg and msg["event"] == event_name:
                            #print(f"Detected event: {event_name}")
                            return msg
                    except json.JSONDecodeError:
                        continue
    print(f"Timeout waiting for event: {event_name}")
    return None
