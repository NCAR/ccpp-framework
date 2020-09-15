"""Definition of the state machine used by the CCPP"""

# CCPP framework imports
from state_machine import StateMachine

_INIT_ST = r"(?:(?i)init(?:ial(?:ize)?)?)"
_FINAL_ST = r"(?:(?i)final(?:ize)?)"
_RUN_ST = r"(?:(?i)run)"
_TS_INIT_ST = r"(?:(?i)timestep_init(?:ial(?:ize)?)?)"
_TS_FINAL_ST = r"(?:(?i)timestep_final(?:ize)?)"

# Allowed CCPP transitions
# pylint: disable=bad-whitespace
RUN_PHASE_NAME = 'run'
CCPP_STATE_MACH = StateMachine((('initialize', 'uninitialized',
                                 'initialized', _INIT_ST),
                                ('timestep_initial', 'initialized',
                                 'in_time_step', _TS_INIT_ST),
                                (RUN_PHASE_NAME, 'in_time_step',
                                 'in_time_step', _RUN_ST),
                                ('timestep_final', 'in_time_step',
                                 'initialized', _TS_FINAL_ST),
                                ('finalize', 'initialized',
                                 'uninitialized', _FINAL_ST)))
# pylint: enable=bad-whitespace
