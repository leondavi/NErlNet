from flask import Flask
from flask_restful import Api, Resource

reciever = Flask(__name__)
recieverApi = Api(reciever)

if __name__ == "__main__":
    reciever.run(debug=True)