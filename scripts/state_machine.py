#!/usr/bin/env python
#

"""Classes and methods to implement a simple state machine."""

# Python library imports
import re
from collections import OrderedDict
# CCPP framework imports
from parse_tools import FORTRAN_ID

###############################################################################

class StateMachine(object):
    """Class and methods to implement a simple state machine.
    Note, a collections.UserDict would be nice here but it is not in python 2.
    >>> StateMachine()
    StateMachine()
    >>> StateMachine([('ab','a','b','a')])
    StateMachine(ab)
    >>> StateMachine([('ab','a','b','a'),('cd','c','d','c')])
    StateMachine(ab, cd)
    >>> StateMachine([('ab','a','b','a')]).add_transition('cd','c','d','c')

    >>> StateMachine([('ab','a','b','a')])['cd'] = ('c','d','c')
    >>> StateMachine([('ab','a','b','a'),('cd','c','d','c')]).transitions()
    ['ab', 'cd']
    >>> StateMachine([('ab','a','b','a')]).initial_state('ab')
    'a'
    >>> StateMachine([('ab','a','b','a')]).final_state('ab')
    'b'
    >>> StateMachine([('ab','a','b','a')]).transition_regex('ab')
    re.compile('a$')
    >>> StateMachine([('ab','a','b','a')]).function_match('foo_a', transition='ab')
    ('foo', 'a', 'ab')
    >>> StateMachine([('ab','a','b',r'ax?')]).function_match('foo_a', transition='ab')
    ('foo', 'a', 'ab')
    >>> StateMachine([('ab','a','b',r'ax?')]).function_match('foo_ax', transition='ab')
    ('foo', 'ax', 'ab')
    >>> StateMachine([('ab','a','b','a')]).function_match('foo_ab', transition='ab')
    (None, None, None)
    >>> StateMachine([('ab','a','b','a'),('cd','c','d','c')]).function_match('foo_c')
    ('foo', 'c', 'cd')
    >>> StateMachine([('ab','a','b',r'ax?')]).transition_match('a')
    'ab'
    >>> StateMachine([('ab','a','b',r'ax?')]).transition_match('ax')
    'ab'
    >>> StateMachine([('ab','a','b',r'ax?')]).transition_match('axx')

    >>> StateMachine([('ab','a','b','a')]).transition_match('ab')

    >>> StateMachine([('ab','a','b','a'),('cd','c','d','c')]).transition_match('c')
    'cd'
    >>> StateMachine((('ab','a','b','a'),)).add_transition('ab','c','d','c') #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ValueError: ERROR: transition, 'ab', already exists
    >>> StateMachine((('ab','a','b','a'))) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ValueError: Invalid initial_data transition ('ab'), should be of the form (name, inital_state, final_state, regex).
    >>> StateMachine([('ab','a','b','a')])['cd'] = ('c','d') #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ValueError: Invalid transition (('c', 'd')), should be of the form (inital_state, final_state, regex).
    """

    def __init__(self, initial_data=None):
        """Implement a finite state machine.
        <initial_data> is an iterable where each item has four elements:
        (transition_name, <initial_state>, <final_state>, <transition_regex>)
        <transition_regex> is a string representing allowable names for
        functions which form part of the transition action.
        """
        # Implement the State Transition Table as a tuple and use accessors
        self.__stt__ = OrderedDict()
        if initial_data is not None:
            # Note that we need to add states with longer regular expressions
            # before short ones so that we match correctly.
            for trans in sorted(initial_data, key=lambda x: len(x[3]) if len(x) > 3 else 0, reverse=True):
                if len(trans) != 4:
                    raise ValueError("Invalid initial_data transition ({}), should be of the form (name, inital_state, final_state, regex).".format(trans))
                # end if
                self.add_transition(trans[0], trans[1], trans[2], trans[3])
            # end for
        # end if

    def add_transition(self, name, init_state, final_state, regex):
        """Add a transition to this state machine.
        See __setitem__ for implementation details."""
        self[name] = (init_state, final_state, regex)

    def transitions(self):
        """Return a list of transition names"""
        return list(self.__stt__.keys())

    def initial_state(self, transition):
        """Return the initial (before) state for <transition>"""
        return self.__stt__[transition][0]

    def final_state(self, transition):
        """Return the final (after) state for <transition>"""
        return self.__stt__[transition][1]

    def transition_regex(self, transition):
        """Return the compiled regex for <transition>"""
        return self.__stt__[transition][2]

    def function_regex(self, transition):
        """Return the compiled functino regex for <transition>"""
        return self.__stt__[transition][3]

    def transition_match(self, test_str, transition=None):
        """Return the matched transition, if found.
        """
        match_trans = None
        if transition is None:
            trans_list = self.transitions()
        else:
            trans_list = [transition]
        # end if
        for trans in trans_list:
            regex = self.transition_regex(trans)
            match = regex.match(test_str)
            if match is not None:
                match_trans = trans
                break
            # end if
        # end for
        return match_trans

    def function_match(self, test_str, transition=None):
        """Return a function ID, transition identifier, and matched
        transition if found.
        If <transition> is None, look for a match in any transition,
        otherwise, only look for a specific match to that transition.
        """
        if transition is None:
            trans_list = self.transitions()
        else:
            trans_list = [transition]
        # end if
        func_id = None
        trans_id = None
        match_trans = None
        for trans in trans_list:
            regex = self.function_regex(trans)
            match = regex.match(test_str)
            if match is not None:
                func_id = match.group(1)
                trans_id = match.group(2)
                match_trans = trans
                break
            # end if
        # end for
        return func_id, trans_id, match_trans

    def __getitem__(self, key):
        return self.__stt__[key]

    def __setitem__(self, key, value):
        if key in self.__stt__:
            raise ValueError("ERROR: transition, '{}', already exists".format(key))
        # end if
        if len(value) != 3:
            raise ValueError("Invalid transition ({}), should be of the form (inital_state, final_state, regex).".format(value))
        # end if
        regex = re.compile(value[2] + r"$")
        function = re.compile(FORTRAN_ID + r"_(" + value[2] + r")$")
        self.__stt__[key] = (value[0], value[1], regex, function)

    def __delitem__(self, key):
        del self.__stt__[key]

    def __iter__(self):
        return iter(self.__stt__)

    def __len__(self):
        return len(self.__stt__)

    def __str__(self):
        return "StateMachine({})".format(", ".join(self.transitions()))

    def __repr__(self):
        return str(self)

###############################################################################
if __name__ == "__main__":
    import doctest
    doctest.testmod()
