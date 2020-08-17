#!/usr/bin/env python

from flask import Flask, jsonify, render_template
from data import NAMES, SELECT

app = Flask(__name__)


@app.route("/")
def home():
    return render_template("index.html", names=NAMES)


@app.route("/person/<string:name>")
def person(name):
    try:
        return render_template("person.html", name=name, data=SELECT[name], error=False)
    except KeyError:
        return render_template("person.html", name=name, error=True), 400


@app.route("/api/<string:name>")
def api(name):
    try:
        return jsonify(SELECT[name])
    except KeyError:
        return {"error": f"Unknown person {name}"}, 400
