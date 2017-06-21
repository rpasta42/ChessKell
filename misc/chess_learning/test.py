#!/usr/bin/python3

import sh, json



def get_user(name):
   return parse_section('user', name)


def get_tournament(tid, page=None):
   return parse_section('tournament', tid, page)


def parse_section(jsonStr):
   return json.loads(jsonStr)

#print(sh.curl('https://lichess.org/api/tournament/x5WNIngd?page=1'))

def get_api(section, sect_id, page_num=None):

   page = ''
   if page_num is not None:
      page = '?page=' + str(page_num)

   return sh.curl('https://lichess.org/api/' + sect_id + '/' + page


# 'https://lichess.org/api/tournament/x5WNIngd?page=1'
