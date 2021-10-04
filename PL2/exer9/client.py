import requests, math, sys
from bs4 import BeautifulSoup

def calcSol(n, k, p):
    return math.comb(n, k) % p

URL = 'http://127.0.0.1:5000'
if len(sys.argv) > 1:
    URL = sys.argv[1]
session = requests.Session()
page = session.get(URL)
soup = BeautifulSoup(page.content, 'html.parser')
rounds = int(soup.find(id='totalQuestions').text)

#play the game
for _ in range(rounds):
    questionNum = int(soup.find(id='questionNum').text)
    n = int(soup.find(id='n').text)
    k = int(soup.find(id='k').text)
    p = int(soup.find(id='p').text)
    print(f'Question {questionNum}:')
    print(f'Calculate C({n},{k}) modulo {p}:')
    print(f'{calcSol(n, k, p)}')
    res = session.post(URL, data={'answer': calcSol(n, k, p)})
    soup = BeautifulSoup(res.content, 'html.parser')
    answer = soup.find(id='answer').text
    if answer == "Correct":
        print('Correct answer!')
    else:
        print('Wrong answer...')
    score = soup.find(id='score')
    if score:
        print('End of game')
        print(f'Score: {score.text}')
    else:
        page = session.get(URL)
        soup = BeautifulSoup(page.content, 'html.parser')
