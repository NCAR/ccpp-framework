#!/usr/bin/env python

def extract_arguments(call):
    # Replace any leftover multiple whitespaces with single whitespaces
    call = ' '.join(call.split())
    # Extract arguments part of call
    args_start = call.find('(')
    args_end = call.rfind(')')
    args_str = (call[args_start+1:args_end]).strip()
    args_str_formatted = ''
    # Remove whitespaces from parts of the argument list that are inside brackets
    # and add missing whitespaces after the comma of each variable in the call
    open_brackets = 0
    for i in xrange(len(args_str)):
        # First check if we are inside or outside of a bracketed area
        if args_str[i] == '(':
            open_brackets += 1
        elif args_str[i] == ')':
            if open_brackets < 1:
                raise Exception('Error parsing {0}, found closing bracket w/o matching opening bracket'.format(call))
            open_brackets -= 1
        # Remove whitespaces inside bracketed areas
        if open_brackets > 0 and args_str[i] == ' ':
            continue
        args_str_formatted += args_str[i]
        # Add missing whitespace after comma between arguments
        if open_brackets == 0 and i<len(args_str)-1 and args_str_formatted[-1] == ',' and not args_str[i+1] == ' ':
            args_str_formatted += ' '
    # Split argument list
    args = [x for x in args_str_formatted.split(', ')]
    return args

def parse_subroutine_call(file, subroutine):
    parse = False
    calls = []
    with open(file) as f:
        contents = []
        for line in f.readlines():
            line = line.strip()
            # Skip comments and empty lines
            if line.startswith('!') or line == '':
                continue
            # Replace tabs with single whitespaces
            line = line.replace('\t', ' ')
            # Replace multiple whitespaces with one whitespace
            line = ' '.join(line.split())
            # Remove inline comments
            if '!' in line:
                line = line[:line.find('!')].strip()
            contents.append(line)
        for line in contents:
            #print subroutine + ' : "' + line + '"'
            if 'call {0}'.format(subroutine) in line.lower():
            #DH* case sensitive? if 'call {0}'.format(subroutine) in line:
                parse = True
                call = line.replace('&', ' ')
                count_opening_brackets = line.count('(')
                count_closing_brackets = line.count(')')
                # Entire call on one line
                if count_opening_brackets > 0 and count_closing_brackets == count_opening_brackets:
                    parse = False
                    arguments = extract_arguments(call)
                    calls.append(arguments)
            elif parse:
                call += line.replace('&', ' ')
                count_opening_brackets += line.count('(')
                count_closing_brackets += line.count(')')
                # Call over multiple lines
                if (count_closing_brackets == count_opening_brackets) and (count_closing_brackets>0):
                    parse = False
                    arguments = extract_arguments(call)
                    calls.append(arguments)
    #print "file, subroutine, calls:", file, subroutine, calls, [len(arguments) for arguments in calls]
    return calls
