


class EntityDB:
    def __init__(self):
        self.messages_received = 0
        self.messages_sent = 0
        self.bytes_received = 0
        self.bytes_sent = 0
        self.bad_messages = 0
        self.dropped_messages = 0

class RouterDB(EntityDB):
    def __init__(self):
        pass

class ClientDB(EntityDB):
    def __init__(self):
        pass

class SourceDB(EntityDB):
    def __init__(self):
        pass                         