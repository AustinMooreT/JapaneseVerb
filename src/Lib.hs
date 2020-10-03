{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Lib  where

--TODO/NOTE very little error checking is happening in this program.

data Verb = Verb
  {
    _kanji      :: String,
    _furigana   :: String,
    _definition :: String
  }

--TODO missing some sounds
hirigana :: [(Char, String)]
hirigana = [('あ',"a") ,('い',"i")  ,('う',"u")  ,('え',"e") ,('お', "o") ,
            ('か',"ka"),('き',"ki") ,('く',"ku") ,('け',"ke"),('こ', "ko"),
            ('が',"ga"),('ぎ',"gi") ,('ぐ',"gu") ,('げ',"ge"),('ご', "go"),
            ('さ',"sa"),('し',"shi"),('す',"su") ,('せ',"se"),('そ', "so"),
            ('ざ',"za"),('じ',"ji") ,('ず',"zu") ,('ぜ',"ze"),('ぞ', "zo"),
            ('た',"ta"),('ち',"chi"),('つ',"tsu"),('て',"te"),('と', "to"),
            ('だ',"da"),('ぢ',"di") ,('づ',"dzu"),('で',"de"),('ど', "do"),
            ('な',"na"),('に',"ni") ,('ぬ',"nu") ,('ね',"ne"),('の', "no"),
            ('は',"ha"),('ひ',"hi") ,('ふ',"fu") ,('へ',"he"),('ほ', "ho"),
            ('ば',"ba"),('び',"bi") ,('ぶ',"bu") ,('べ',"be"),('ぼ', "bo"),
            ('ぱ',"pa"),('ぴ',"pi") ,('ぷ',"pu") ,('ぺ',"pe"),('ぽ', "po"),
            ('ま',"ma"),('み',"mi") ,('む',"mu") ,('め',"me"),('も', "mu"),
            ('や',"ya"),('ゆ',"yu") ,('よ',"yo") ,
            ('ら',"ra"),('り',"ri") ,('る',"ru") ,('れ',"re"),('ろ', "ro"),
            ('わ',"wa"),('ゐ',"wi") ,('ゑ',"we") ,('を',"wo"),
            ('ん',"n")]

hiriganaToRomanji :: Char -> String
hiriganaToRomanji hir = snd . head $ filter (\(x,_) -> x == hir) hirigana

romanjiToHirigana :: String -> Char
romanjiToHirigana rom = fst . head $ filter (\(_,x) -> x == rom) hirigana

getLastVowelSound :: String -> Char
getLastVowelSound str = romanjiToHirigana . (: []) . last . hiriganaToRomanji . last $ str

getConsonant :: Char -> Char
getConsonant hir = head $ hiriganaToRomanji hir

modifyVowelSound :: Char -> Char -> Char
modifyVowelSound hir vowel
  | getConsonant hir == 's' && vowel == 'i' = 'し'
  | getConsonant hir == 't' && vowel == 'i' = 'ち'
  | getConsonant hir == 't' && vowel == 'u' = 'つ'
  | getConsonant hir == 'd' && vowel == 'u' = 'づ'
  | otherwise = romanjiToHirigana ((init . hiriganaToRomanji $ hir) ++ [vowel])

--TODO enumerate irregulars
irregulars = []

isIrregular :: String -> Bool
isIrregular str = elem str irregulars

isIchidan :: String -> Bool
isIchidan str = last str == 'る' &&
                (getLastVowelSound $ init str) == 'い' ||
                (getLastVowelSound $ init str) == 'え'

-- | TODO BEGIN irregular functions
politeIrregular :: String -> String
politeIrregular = id

dictionaryNegativeImperativeIrregular :: String -> String
dictionaryNegativeImperativeIrregular = id

-- ^ TODO END Irregular functions

politeForm :: String -> String
politeForm str
  | isIrregular str = politeIrregular str
  | isIchidan   str = init str ++ "ます"
  | otherwise       = init str ++ [(modifyVowelSound (last str) 'i')] ++ "ます"

--dictionaryNegativeImperative :: String -> String
--dictionaryNegativeImperative str
--  | isIrregular str = dictionaryNegativeImperativeIrregular str
--  | isIchidan   str = init str ++ "な"-
--  | otherwise
