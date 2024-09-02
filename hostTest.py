from flask import Flask, jsonify, request
import subprocess

app = Flask(__name__)

# 创建一个字典来保存游戏的状态
games = {}
game_id = 1

@app.route('/game', methods=['POST'])
def startGame():
    global game_id

    # 使用 Popen 启动游戏，并将 stdin、stdout 和 stderr 连接到管道
    process = subprocess.Popen(['make', 'game'], 
                               stdin=subprocess.PIPE, 
                               stdout=subprocess.PIPE, 
                               text=True, 
                               bufsize=1,
                               universal_newlines=True)
    
    # 保存游戏进程的引用
    games[game_id] = process

    # 从游戏进程读取输出
    output = process.stdout.readline().strip()

    # 返回输出给客户端
    response = {
        'game_id': game_id,
        'message': output
    }
    game_id += 1
    return jsonify(response)

@app.route('/game/<int:id>/play', methods=['POST'])
def playCard(id):
    card_number = request.json.get('card_number')
    process = games.get(id)

    if not process:
        return jsonify(error="Game not found"), 404

    # 将用户的输入写入游戏进程
    process.stdin.write(str(card_number) + '\n')
    process.stdin.flush()

    # 从游戏进程读取输出
    output = process.stdout.readline().strip()

    # 返回输出给客户端
    return jsonify(message=output)

if __name__ == "__main__":
    app.run(debug=True)
