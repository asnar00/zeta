# Runtime harness: bridges OS to zero's main task
import sys

if __name__ == "__main__":
    try:
        result = task_main__string(sys.argv[1:])
        # if main is a generator (yields output), print each line
        if hasattr(result, '__next__'):
            for line in result:
                print(line)
    except NameError:
        pass  # no main task defined
