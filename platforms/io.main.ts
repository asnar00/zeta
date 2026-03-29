// Runtime harness: bridges OS to zero's main task
try {
    for (const line of task_main__string(process.argv.slice(2))) {
        console.log(line);
    }
} catch (e) {
    // no main task defined
}
