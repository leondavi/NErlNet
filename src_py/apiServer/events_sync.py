import os
from time import sleep, monotonic
from logger import *
class EventSync():
    # api server events 
    SEND_JSONS = 0
    RESTART = 4
    # experiment flow events
    UPDATE_CSV = 1
    UPDATE_PHASE = 2
    START_CASTING = 3
    # communication stats
    COMMUNICATION_STATS = 6
    # error events
    MAIN_SERVER_ERROR = 5

    DONE = 1
    WAIT = 2
    INIT = 3
    def __init__(self):
        self.done_actions_dict = self.generate_done_actions_dict()
        self.tracking_dict = self.generate_tracking_dict()
        self.progress_log_interval_sec = self._parse_progress_log_interval_sec()

    def _parse_progress_log_interval_sec(self) -> float:
        raw_interval = os.getenv("NERLNET_EVENT_WAIT_PROGRESS_SEC", "15")
        try:
            interval = float(raw_interval)
            if interval <= 0:
                raise ValueError("interval must be > 0")
            return interval
        except (TypeError, ValueError):
            LOG_WARNING(
                "Invalid NERLNET_EVENT_WAIT_PROGRESS_SEC='%s'; using 15s",
                raw_interval
            )
            return 15.0

    def get_event_done(self, event_done_str: str):
        assert event_done_str in self.done_actions_dict
        return self.done_actions_dict[event_done_str] 

    def set_event_wait(self, event):
        assert event in self.done_actions_dict.values() 
        assert event in self.tracking_dict
        self.tracking_dict[event] = self.WAIT
        return True
            
    def sync_on_event(self, event, timeout_sec=None, poll_sec=0.05, wait_label=""):
        assert event in self.done_actions_dict.values()
        assert event in self.tracking_dict
        start_time = monotonic()
        next_progress_log_at = start_time + self.progress_log_interval_sec
        label = wait_label if wait_label else f"event={event}"
        while self.tracking_dict[event] == self.WAIT:
            if self.get_error_status():
                raise RuntimeError(f"Main Server signaled error while waiting for {label}")
            if timeout_sec is not None and timeout_sec >= 0:
                elapsed = monotonic() - start_time
                if elapsed > timeout_sec:
                    raise TimeoutError(
                        f"Timed out waiting for {label} after {timeout_sec:.1f}s"
                    )
            now = monotonic()
            if now >= next_progress_log_at:
                LOG_INFO(f"Still waiting for {label}; elapsed={now - start_time:.1f}s")
                next_progress_log_at = now + self.progress_log_interval_sec
            sleep(poll_sec)
    
    def get_event_status(self, event):
        assert event in self.tracking_dict
        return self.tracking_dict[event]

    def get_error_status(self):
        error_events = [self.MAIN_SERVER_ERROR]
        for event in error_events:
            if self.tracking_dict[event] == self.DONE:
                LOG_ERROR(f"Main Server Error")
                return True
        return False

    def set_event_done(self,event):
        assert event in self.done_actions_dict.values()
        assert event in self.tracking_dict
        current_state = self.tracking_dict[event]
        if current_state == self.DONE:
            return
        if current_state != self.WAIT:
            LOG_WARNING(
                f"Ignoring stale/unexpected event_done for event={event} "
                f"state={current_state}"
            )
            return
        self.tracking_dict[event] = self.DONE

    def reset(self):
        self.done_actions_dict = self.generate_done_actions_dict()
        self.tracking_dict = self.generate_tracking_dict()

    def generate_done_actions_dict(self):
        done_actions_dict = {
            # recieve actions from main server
            "received_jsons_done" : self.SEND_JSONS, # means devices setup is done
            "update_csv_done" : self.UPDATE_CSV,
            "update_phase_done" : self.UPDATE_PHASE,
            "start_casting_done" : self.START_CASTING,
            "restart_done" : self.RESTART,  # Todo Guy please implement it
            "communication_stats_done" : self.COMMUNICATION_STATS,
            "main_server_error" : self.MAIN_SERVER_ERROR,
            "parallel_abort" : self.MAIN_SERVER_ERROR,

        }
        return done_actions_dict

    def generate_tracking_dict(self):
        tracking_dict = {
            self.SEND_JSONS: self.INIT,
            self.UPDATE_CSV: self.INIT,
            self.UPDATE_PHASE: self.INIT,
            self.START_CASTING: self.INIT,
            self.RESTART: self.INIT,
            self.COMMUNICATION_STATS: self.INIT,
            self.MAIN_SERVER_ERROR: self.WAIT # ERROR EVENTS ALWAYS START IN WAIT STATE
           
        }
        return tracking_dict

   
