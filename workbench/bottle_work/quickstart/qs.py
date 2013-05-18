#!/usr/bin/env python

from bottle import Bottle, route, run, static_file, template

app = Bottle()

@app.route("/")
@app.route("/hello/<name>")
def greet(name="Stranger"):
    return template("Hello, {{name}}, how are you?", name=name)

@app.route("/hello")
def hello():
    return "Hello World!"

@app.route("/gre")
def gre():
    return "Hi, GRE"


@app.route("/object/<id:int>")
def callback(id):
    assert isinstance(id, int)

@app.route("/show/<name:re:[a-zA-Z]*>")
def callback(name):
    assert name.isalpha()

@app.route("/static/<path:path>")
def callback(path):
    return static_file(path, root="/")

if __name__ == "__main__":
    run(app, reloader=True, host="localhost", port=8080)
