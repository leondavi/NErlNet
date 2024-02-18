
from time import sleep

class EventSync():
    SEND_JSONS = 0
    UPDATE_CSV = 1
    UPDATE_PHASE = 2
    START_CASTING = 3
    TERMINATE = 4

    DONE = 1
    WAIT = 2
    INIT = 3
    def __init__(self):
        self.done_actions_dict = self.generate_done_actions_dict()
        self.tracking_dict = self.generate_tracking_dict()


    def get_event_done(self, event_done_str: str):
        assert event_done_str in self.done_actions_dict
        return self.done_actions_dict[event_done_str] 

    def set_event_wait(self, event):
        assert event in self.done_actions_dict.values() 
        assert event in self.tracking_dict
        if self.tracking_dict[event] == self.INIT:
            self.tracking_dict[event] = self.WAIT
            return True
        return False
            
    def sync_on_event(self, event):
        assert event in self.done_actions_dict.values()
        assert event in self.tracking_dict
        while(self.tracking_dict[event] == self.WAIT):
            sleep(0.005)
    
    def get_event_status(self, event):
        assert event in self.tracking_dict
        return self.tracking_dict[event]

    def set_event_done(self,event):
        assert event in self.done_actions_dict.values()
        assert event in self.tracking_dict
        assert self.tracking_dict[event] == self.WAIT
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
            "terminate_done" : self.TERMINATE
        }
        return done_actions_dict

    def generate_tracking_dict(self):
        tracking_dict = {
            self.SEND_JSONS: self.INIT,
            self.UPDATE_CSV: self.INIT,
            self.UPDATE_PHASE: self.INIT,
            self.CLIENT_ACK: self.INIT,
            self.START_CASTING: self.INIT,
            self.TERMINATE: self.INIT
        }
        return tracking_dict

   