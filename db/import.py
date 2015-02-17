from csh.cshldap import LDAP, USERS
import ldap
from dateutil import parser
from datetime import datetime
import psycopg2

db_conn = psycopg2.connect(database='eval',
                           user='gambogi')
ldap_conn = LDAP('pval', 'vaginal83&talkie', app=True).ldap_conn

entries = ldap_conn.search_s(USERS, ldap.SCOPE_SUBTREE, '(uid=*)',
                             ['cn', 'uid', 'entryUUID',
                              'memberSince', 'housingPoints'])

cur = db_conn.cursor()

insert = []
for entry in entries:
    for key, value in entry[1].iteritems():
        entry[1][key] = value[0]
    print entry
    try:
        entry[1]['memberSince'] = parser.parse(entry[1]['memberSince'])
    except:
        entry[1]['memberSince'] = datetime.now()
    insert.append((entry[1]['entryUUID'], entry[1]['uid'], entry[1]['cn'],
                  entry[1]['memberSince'], False, 'alumni', entry[1]['housingPoints']))

cur.executemany('INSERT INTO member '
                '(uuid, username, commonname, join_time, resident, membership, housing_points)'
                ' VALUES (%s, %s, %s, %s, %s, %s, %s);',
                insert)
db_conn.commit()
