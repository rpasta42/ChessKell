#!/usr/bin/python3

#dependency:
#sudo pip3 install python-chess

import sh, json
import chess

def parse_section(jsonStr):
   return json.loads(str(jsonStr))

#print(sh.curl('https://lichess.org/api/tournament/x5WNIngd?page=1'))

def get_api(section, sect_id, page_num=None, with_moves=None):

   have_params = False

   page = ''
   if page_num is not None:
      have_params = True
      page = 'page=' + str(page_num)


   moves_param = ''
   if with_moves is not None:
      have_params = True
      moves_param = 'with_moves=' + str(with_moves)

   params = ''
   if have_params:
      params = '?'
      if page != '':
         params += page
         if moves_param != '':
            params += '&'

      if moves_param != '':
         params += moves_param


   req_str = 'https://lichess.org/api/' + section + '/' + sect_id + params
   print(req_str)
   return sh.curl(req_str)



def get_user(name):
   return get_api('user', name)

def get_user_games(name, page_num=1):
   return get_api('user', name + '/games', page_num)

def get_tournament(tid, page=None):
   return get_api('tournament', tid, page)


def parse_user_games(name):
   ret = parse_section(str(get_user_games(name)))
   games = ret['currentPageResults']
   game_urls = list(map(lambda x: x['url'], games))

   game_ids = list(map(lambda x: x.split('/')[-2], game_urls))

   print(game_ids)
   #return game_urls
   return games



def get_game(gid):
   game = parse_section(get_api('game', gid, with_moves = 1))
   return game

def convert_game(moves_str):
   moves_lst = moves_str.split(' ')
   board = chess.Board()

   good_moves = []

   i = 0
   while i < len(moves_lst):

      curr_move = board.push_san(moves_lst[i])
      print(curr_move)
      good_moves.append(curr_move.uci())

      i += 1

   return good_moves


#print(get_user_games('rpasta42', 2))
#x = parse_user_games('rpasta42')

y = get_game('tzc0UsMS')
z = convert_game(y['moves'])

# 'https://lichess.org/api/tournament/x5WNIngd?page=1'
