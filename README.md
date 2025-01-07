# これはなに
2024年7月に開催されたMIS39の[実行委員会企画](https://plaza.umin.ac.jp/mis/39/plan.html)用の各種ファイルです。

# Ⅰ. 配布データに含まれているファイル
この配布データには，以下の4種類のファイルが含まれています。

- bookprice.csv
- visitnum.csv
- MIS39_bookprice.ipynb
- MIS39_visitnum.ipynb

「.csv」ファイルは，分析用のサンプルデータです。
「.ipynb」ファイルは，オンラインのプログラミング実行環境「Google Colaboratory」でサンプルデータの分析をRで実行するためのソースコードです。GoogleDriveの任意のフォルダに保存したこのファイルを開くと，分析を実行することができます。利用方法は以下の動画(BGMあり)でも紹介しています。

- MIS39_R分析準備(YouTube)
  - https://youtu.be/FK0DfuWRoAI

# Ⅱ. サンプルデータの説明
### 【bookprice.csv】
- とある図書館の，800冊分の所蔵資料データです。
- 以下の情報を含んでいます。
	- WA, WB, WD, WG, WOの5分類(class)
	- 価格(price), ページ数(pages), 判型(format_name,cm2)
	- カラー情報(color), 単著かどうか(author), 言語(lang)
- 以下一部抜粋です。

| id  | class | price | pages | format_name | format_cm2 | color  | author | lang |
| :-: | :---: | :---: | :---: | :---------: | :--------: | :----: | :----: | :--: |
|  1  |  WA   | 2231  |  184  |     A4      |    623     |  mono  | single | jpn  |
|  2  |  WB   | 3500  |  237  |     B5      |    468     | Color  | multi  | jpn  |
|  3  |  WD   | 3047  |  247  |     A5      |    311     | LimCol | multi  | eng  |
|  4  |  WB   | 2220  |  206  |     A5      |    311     | Color  | multi  | jpn  |
|  5  |  WO   | 4981  |  240  |     A4      |    623     | Color  | multi  | jpn  |

### 【visitnum.csv】
- とある図書館の，11年(132ヶ月分)の月別の入館者数データ(時系列データ)です。
- 以下の情報を含んでいます。
	- 年(year), 月(month)
	- 総数(all), 内数として学生(student), 教職員(faculty)
- 以下一部抜粋です。

| year | month | all  | student | faculty |
| :--: | :---: | :--: | :-----: | :-----: |
| 2012 |   4   | 3049 |  2812   |   237   |
| 2012 |   5   | 2279 |  2074   |   205   |
| 2012 |   6   | 2365 |  2103   |   262   |

# Ⅲ. 分析例
### 【bookprice.csv】
1. 概要把握
	- 価格(price)の概要把握 : 基本統計量，ヒストグラム
	- 分類(class)別データ数 : クロス集計表
2. 分類によって価格は違うか？
	- 分類別の価格比較 : 基本統計量，箱ひげ図
	- どの分類でも(平均)価格は同じか？: 一元配置分散分析
	- どの分類間で(平均)価格に差があるか？: 多重比較検定
3. 価格に影響を与えているものはなにか？
	- 各変数と価格との関係 : 相関係数
	- 資料の要素から価格を予測 : 重回帰分析
### 【visitnum.csv】
- 利用傾向の変動や年間利用の特徴が知りたい。社会状況や自館の取組が入館者数に及ぼした影響が知りたい。→ 時系列データの分解。
1. 傾向変動(T) : 移動平均
	- 中長期の変動の傾向を確認
2. 季節変動・循環変動(S・C)：周期ごとの平均
	- 変動の周期性を確認
3. 不規則変動(I)：残差
	- それ以外のその時々の変動を確認
