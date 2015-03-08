from bottle import Bottle, debug, route, run, request
from bottle.ext import sqlalchemy as bottlesql
from sqlalchemy import create_engine
import sqlalchemy.engine
from ConfigParser import ConfigParser
from csh import cshldap
import psycopg2
import os
from evals import event, member, packet, project, evaluation, schema

def init():
    config = readConfig()
    ldap = cshldap.LDAP(user=config.get('ldap', 'user'),
                        password=config.get('ldap', 'password'),
                        app=True).ldap_conn
    app = Bottle()
    plugin = createPlugin(config)
    app.install(plugin)

    return app, ldap

def readConfig():
    config = ConfigParser()
    configpath = os.getenv('EVAL_CONFIG', os.path.expanduser('~/.evals.cfg'))
    config.read(configpath)
    return config

def createPlugin(config):
    pg = lambda x: config.get('postgres', x)
    url = sqlalchemy.engine.url.URL('postgresql+psycopg2',
                                    username=pg('username'),
                                    password=pg('password'),
                                    host=pg('host'),
                                    database=pg('database'))
    print url
    engine = create_engine(url)
    return bottlesql.Plugin(engine)

app, ldap = init()

@app.route('/')
def index(db):
    print request.headers
    webauth_user = request.headers['X-Webauth-User']
    return member.Member(LDAP, uid=webauth_user).asjson()


@app.route('/member/<uid>')
def member_route(uid, db):
    print request.headers.keys()
    m = member.Member(uid, db, LDAP)
    return m.asjson()

if __name__ == '__main__':
    debug(True)
    app.run(host='0.0.0.0', port=3000, reloader=True)
