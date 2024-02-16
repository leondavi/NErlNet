


class EventSync():
    UPDATE_CSV = 1
    UPDATE_PHASE = 2
    START_CASTING = 3

    DONE = 1
    WAIT = 2
    INIT = 3
    def __init__(self):
        self.ack_actions_dict = self.generate_ack_actions_dict()
        self.tracking_dict = self.generate_tracking_dict()

    def set_event_wait(self, event):
        assert event in self.ack_actions_dict 
        assert event in self.tracking_dict
        if tracking_dict[event] == self.INIT:
            tracking_dict[event] = self.WAIT
            return True
        return False
            
    def sync_on_event(self, event):
        assert event in self.ack_actions_dict
        assert event in self.tracking_dict
        if tracking_dict[event] == self.WAIT:
            return True
        return False

    def set_event_done(self,event):
        assert event in self.ack_actions_dict
        assert event in self.tracking_dict
        assert tracking_dict[event] == self.WAIT
        tracking_dict[event] = self.DONE

    def reset(self):
        self.ack_actions_dict = self.generate_ack_actions_dict()
        self.tracking_dict = self.generate_tracking_dict()

    def generate_ack_actions_dict(self):
        tracking_dict = {
            self.UPDATE_CSV: ("update_csv", "update_csv_done"),
            self.UPDATE_PHASE: ("update_phase", "update_phase_done"),
            self.START_CASTING: ("start_casting", "start_casting_done")
        }
        return tracking_dict

    def generate_tracking_dict(self):
        tracking_dict = {
            self.UPDATE_CSV: self.INIT,
            self.UPDATE_PHASE: self.INIT,
            self.START_CASTING: self.INIT
        }
        return tracking_dict

   