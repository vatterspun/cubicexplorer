//******************************************************************************
//  CubicExplorer                                                                             
//  Version: 0.90                                                                             
//                                                                                            
//  The contents of this file are subject to the Mozilla Public License                       
//  Version 1.1 (the "License"); you may not use this file except in                          
//  compliance with the License. You may obtain a copy of the License at                      
//  http://www.mozilla.org/MPL/                                                               
//                                                                                            
//  Software distributed under the License is distributed on an "AS IS"
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
//  License for the specific language governing rights and limitations                        
//  under the License.                                                                        
//                                                                                            
//  The Original Code is CE_LanguageCodes.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_LanguageCodes;
// This unit is automatically generated from a language list.

interface

uses Classes;

// Return the English name of the language specified as 2 or 5 char
// language codes, e.g. 'en' or 'en_GB'.
function GetLanguageName(LangCode:string): string;

procedure GetLanguageNames(AList: TStrings);

function GetLanguageCode(Index: Integer): string;

const
  LanguageCodeCount = 195;

implementation

function GetLanguageName(LangCode:string): string;
begin
  if langcode='aa' then Result:='Afar' else
  if langcode='ab' then Result:='Abkhazian' else
  if langcode='ae' then Result:='Avestan' else
  if langcode='af' then Result:='Afrikaans' else
  if langcode='ak' then Result:='Akan' else
  if langcode='am' then Result:='Amharic' else
  if langcode='an' then Result:='Aragonese' else
  if langcode='ar' then Result:='Arabic' else
  if langcode='as' then Result:='Assamese' else
  if langcode='av' then Result:='Avaric' else
  if langcode='ay' then Result:='Aymara' else
  if langcode='az' then Result:='Azerbaijani' else
  if langcode='ba' then Result:='Bashkir' else
  if langcode='be' then Result:='Belarusian' else
  if langcode='bg' then Result:='Bulgarian' else
  if langcode='bh' then Result:='Bihari' else
  if langcode='bi' then Result:='Bislama' else
  if langcode='bm' then Result:='Bambara' else
  if langcode='bn' then Result:='Bengali' else
  if langcode='bo' then Result:='Tibetan' else
  if langcode='br' then Result:='Breton' else
  if langcode='bs' then Result:='Bosnian' else
  if langcode='ca' then Result:='Catalan' else
  if langcode='ce' then Result:='Chechen' else
  if langcode='ch' then Result:='Chamorro' else
  if langcode='co' then Result:='Corsican' else
  if langcode='cr' then Result:='Cree' else
  if langcode='cs' then Result:='Czech' else
  if langcode='cv' then Result:='Chuvash' else
  if langcode='cy' then Result:='Welsh' else
  if langcode='da' then Result:='Danish' else
  if langcode='de' then Result:='German' else
  if langcode='de_AT' then Result:='Austrian German' else
  if langcode='de_CH' then Result:='Swiss German' else
  if langcode='dv' then Result:='Divehi' else
  if langcode='dz' then Result:='Dzongkha' else
  if langcode='ee' then Result:='Ewe' else
  if langcode='el' then Result:='Greek' else
  if langcode='en' then Result:='English' else
  if langcode='en_AU' then Result:='Australian English' else
  if langcode='en_CA' then Result:='Canadian English' else
  if langcode='en_GB' then Result:='British English' else
  if langcode='en_US' then Result:='American English' else
  if langcode='eo' then Result:='Esperanto' else
  if langcode='es' then Result:='Spanish' else
  if langcode='et' then Result:='Estonian' else
  if langcode='eu' then Result:='Basque' else
  if langcode='fa' then Result:='Persian' else
  if langcode='ff' then Result:='Fulah' else
  if langcode='fi' then Result:='Finnish' else
  if langcode='fj' then Result:='Fijian' else
  if langcode='fo' then Result:='Faroese' else
  if langcode='fr' then Result:='French' else
  if langcode='fr_BE' then Result:='Walloon' else
  if langcode='fy' then Result:='Frisian' else
  if langcode='ga' then Result:='Irish' else
  if langcode='gd' then Result:='Gaelic' else
  if langcode='gl' then Result:='Gallegan' else
  if langcode='gn' then Result:='Guarani' else
  if langcode='gu' then Result:='Gujarati' else
  if langcode='gv' then Result:='Manx' else
  if langcode='ha' then Result:='Hausa' else
  if langcode='he' then Result:='Hebrew' else
  if langcode='hi' then Result:='Hindi' else
  if langcode='ho' then Result:='Hiri Motu' else
  if langcode='hr' then Result:='Croatian' else
  if langcode='ht' then Result:='Haitian' else
  if langcode='hu' then Result:='Hungarian' else
  if langcode='hy' then Result:='Armenian' else
  if langcode='hz' then Result:='Herero' else
  if langcode='ia' then Result:='Interlingua' else
  if langcode='id' then Result:='Indonesian' else
  if langcode='ie' then Result:='Interlingue' else
  if langcode='ig' then Result:='Igbo' else
  if langcode='ii' then Result:='Sichuan Yi' else
  if langcode='ik' then Result:='Inupiaq' else
  if langcode='io' then Result:='Ido' else
  if langcode='is' then Result:='Icelandic' else
  if langcode='it' then Result:='Italian' else
  if langcode='iu' then Result:='Inuktitut' else
  if langcode='ja' then Result:='Japanese' else
  if langcode='jv' then Result:='Javanese' else
  if langcode='ka' then Result:='Georgian' else
  if langcode='kg' then Result:='Kongo' else
  if langcode='ki' then Result:='Kikuyu' else
  if langcode='kj' then Result:='Kuanyama' else
  if langcode='kk' then Result:='Kazakh' else
  if langcode='kl' then Result:='Greenlandic' else
  if langcode='km' then Result:='Khmer' else
  if langcode='kn' then Result:='Kannada' else
  if langcode='ko' then Result:='Korean' else
  if langcode='kr' then Result:='Kanuri' else
  if langcode='ks' then Result:='Kashmiri' else
  if langcode='ku' then Result:='Kurdish' else
  if langcode='kw' then Result:='Cornish' else
  if langcode='kv' then Result:='Komi' else
  if langcode='ky' then Result:='Kirghiz' else
  if langcode='la' then Result:='Latin' else
  if langcode='lb' then Result:='Luxembourgish' else
  if langcode='lg' then Result:='Ganda' else
  if langcode='li' then Result:='Limburgan' else
  if langcode='ln' then Result:='Lingala' else
  if langcode='lo' then Result:='Lao' else
  if langcode='lt' then Result:='Lithuanian' else
  if langcode='lu' then Result:='Luba-Katanga' else
  if langcode='lv' then Result:='Latvian' else
  if langcode='mg' then Result:='Malagasy' else
  if langcode='mh' then Result:='Marshallese' else
  if langcode='mi' then Result:='Maori' else
  if langcode='mk' then Result:='Macedonian' else
  if langcode='ml' then Result:='Malayalam' else
  if langcode='mn' then Result:='Mongolian' else
  if langcode='mo' then Result:='Moldavian' else
  if langcode='mr' then Result:='Marathi' else
  if langcode='ms' then Result:='Malay' else
  if langcode='mt' then Result:='Maltese' else
  if langcode='my' then Result:='Burmese' else
  if langcode='na' then Result:='Nauru' else
  if langcode='nb' then Result:='Norwegian Bokmaal' else
  if langcode='nd' then Result:='Ndebele, North' else
  if langcode='ne' then Result:='Nepali' else
  if langcode='ng' then Result:='Ndonga' else
  if langcode='nl' then Result:='Dutch' else
  if langcode='nl_BE' then Result:='Flemish' else
  if langcode='nn' then Result:='Norwegian Nynorsk' else
  if langcode='no' then Result:='Norwegian' else
  if langcode='nr' then Result:='Ndebele, South' else
  if langcode='nv' then Result:='Navajo' else
  if langcode='ny' then Result:='Chichewa' else
  if langcode='oc' then Result:='Occitan' else
  if langcode='oj' then Result:='Ojibwa' else
  if langcode='om' then Result:='Oromo' else
  if langcode='or' then Result:='Oriya' else
  if langcode='os' then Result:='Ossetian' else
  if langcode='pa' then Result:='Panjabi' else
  if langcode='pi' then Result:='Pali' else
  if langcode='pl' then Result:='Polish' else
  if langcode='ps' then Result:='Pushto' else
  if langcode='pt' then Result:='Portuguese' else
  if langcode='pt_BR' then Result:='Brazilian Portuguese' else
  if langcode='qu' then Result:='Quechua' else
  if langcode='rm' then Result:='Raeto-Romance' else
  if langcode='rn' then Result:='Rundi' else
  if langcode='ro' then Result:='Romanian' else
  if langcode='ru' then Result:='Russian' else
  if langcode='rw' then Result:='Kinyarwanda' else
  if langcode='sa' then Result:='Sanskrit' else
  if langcode='sc' then Result:='Sardinian' else
  if langcode='sd' then Result:='Sindhi' else
  if langcode='se' then Result:='Northern Sami' else
  if langcode='sg' then Result:='Sango' else
  if langcode='si' then Result:='Sinhalese' else
  if langcode='sk' then Result:='Slovak' else
  if langcode='sl' then Result:='Slovenian' else
  if langcode='sm' then Result:='Samoan' else
  if langcode='sn' then Result:='Shona' else
  if langcode='so' then Result:='Somali' else
  if langcode='sq' then Result:='Albanian' else
  if langcode='sr' then Result:='Serbian' else
  if langcode='ss' then Result:='Swati' else
  if langcode='st' then Result:='Sotho, Southern' else
  if langcode='su' then Result:='Sundanese' else
  if langcode='sv' then Result:='Swedish' else
  if langcode='sw' then Result:='Swahili' else
  if langcode='ta' then Result:='Tamil' else
  if langcode='te' then Result:='Telugu' else
  if langcode='tg' then Result:='Tajik' else
  if langcode='th' then Result:='Thai' else
  if langcode='ti' then Result:='Tigrinya' else
  if langcode='tk' then Result:='Turkmen' else
  if langcode='tl' then Result:='Tagalog' else
  if langcode='tn' then Result:='Tswana' else
  if langcode='to' then Result:='Tonga' else
  if langcode='tr' then Result:='Turkish' else
  if langcode='ts' then Result:='Tsonga' else
  if langcode='tt' then Result:='Tatar' else
  if langcode='tw' then Result:='Twi' else
  if langcode='ty' then Result:='Tahitian' else
  if langcode='ug' then Result:='Uighur' else
  if langcode='uk' then Result:='Ukrainian' else
  if langcode='ur' then Result:='Urdu' else
  if langcode='uz' then Result:='Uzbek' else
  if langcode='ve' then Result:='Venda' else
  if langcode='vi' then Result:='Vietnamese' else
  if langcode='vo' then Result:='Volapuk' else
  if langcode='wa' then Result:='Walloon' else
  if langcode='wo' then Result:='Wolof' else
  if langcode='xh' then Result:='Xhosa' else
  if langcode='yi' then Result:='Yiddish' else
  if langcode='yo' then Result:='Yoruba' else
  if langcode='za' then Result:='Zhuang' else
  if langcode='zh' then Result:='Chinese' else
  if langcode='zh-cn' then Result:='Chinese (Simplified)' else
  if langcode='zh-tw' then Result:='Chinese (Traditional)' else
  if langcode='zu' then Result:='Zulu' else
  Result:='';
end;

procedure GetLanguageNames(AList: TStrings);
begin
  AList.Clear;
  AList.BeginUpdate;
  try
    AList.Add('Afar');
    AList.Add('Abkhazian');
    AList.Add('Avestan');
    AList.Add('Afrikaans');
    AList.Add('Akan');
    AList.Add('Amharic');
    AList.Add('Aragonese');
    AList.Add('Arabic');
    AList.Add('Assamese');
    AList.Add('Avaric');
    AList.Add('Aymara');
    AList.Add('Azerbaijani');
    AList.Add('Bashkir');
    AList.Add('Belarusian');
    AList.Add('Bulgarian');
    AList.Add('Bihari');
    AList.Add('Bislama');
    AList.Add('Bambara');
    AList.Add('Bengali');
    AList.Add('Tibetan');
    AList.Add('Breton');
    AList.Add('Bosnian');
    AList.Add('Catalan');
    AList.Add('Chechen');
    AList.Add('Chamorro');
    AList.Add('Corsican');
    AList.Add('Cree');
    AList.Add('Czech');
    AList.Add('Chuvash');
    AList.Add('Welsh');
    AList.Add('Danish');
    AList.Add('German');
    AList.Add('Austrian German');
    AList.Add('Swiss German');
    AList.Add('Divehi');
    AList.Add('Dzongkha');
    AList.Add('Ewe');
    AList.Add('Greek');
    AList.Add('English');
    AList.Add('Australian English');
    AList.Add('Canadian English');
    AList.Add('British English');
    AList.Add('American English');
    AList.Add('Esperanto');
    AList.Add('Spanish');
    AList.Add('Estonian');
    AList.Add('Basque');
    AList.Add('Persian');
    AList.Add('Fulah');
    AList.Add('Finnish');
    AList.Add('Fijian');
    AList.Add('Faroese');
    AList.Add('French');
    AList.Add('Walloon');
    AList.Add('Frisian');
    AList.Add('Irish');
    AList.Add('Gaelic');
    AList.Add('Gallegan');
    AList.Add('Guarani');
    AList.Add('Gujarati');
    AList.Add('Manx');
    AList.Add('Hausa');
    AList.Add('Hebrew');
    AList.Add('Hindi');
    AList.Add('Hiri Motu');
    AList.Add('Croatian');
    AList.Add('Haitian');
    AList.Add('Hungarian');
    AList.Add('Armenian');
    AList.Add('Herero');
    AList.Add('Interlingua');
    AList.Add('Indonesian');
    AList.Add('Interlingue');
    AList.Add('Igbo');
    AList.Add('Sichuan Yi');
    AList.Add('Inupiaq');
    AList.Add('Ido');
    AList.Add('Icelandic');
    AList.Add('Italian');
    AList.Add('Inuktitut');
    AList.Add('Japanese');
    AList.Add('Javanese');
    AList.Add('Georgian');
    AList.Add('Kongo');
    AList.Add('Kikuyu');
    AList.Add('Kuanyama');
    AList.Add('Kazakh');
    AList.Add('Greenlandic');
    AList.Add('Khmer');
    AList.Add('Kannada');
    AList.Add('Korean');
    AList.Add('Kanuri');
    AList.Add('Kashmiri');
    AList.Add('Kurdish');
    AList.Add('Cornish');
    AList.Add('Komi');
    AList.Add('Kirghiz');
    AList.Add('Latin');
    AList.Add('Luxembourgish');
    AList.Add('Ganda');
    AList.Add('Limburgan');
    AList.Add('Lingala');
    AList.Add('Lao');
    AList.Add('Lithuanian');
    AList.Add('Luba-Katanga');
    AList.Add('Latvian');
    AList.Add('Malagasy');
    AList.Add('Marshallese');
    AList.Add('Maori');
    AList.Add('Macedonian');
    AList.Add('Malayalam');
    AList.Add('Mongolian');
    AList.Add('Moldavian');
    AList.Add('Marathi');
    AList.Add('Malay');
    AList.Add('Maltese');
    AList.Add('Burmese');
    AList.Add('Nauru');
    AList.Add('Norwegian Bokmaal');
    AList.Add('Ndebele, North');
    AList.Add('Nepali');
    AList.Add('Ndonga');
    AList.Add('Dutch');
    AList.Add('Flemish');
    AList.Add('Norwegian Nynorsk');
    AList.Add('Norwegian');
    AList.Add('Ndebele, South');
    AList.Add('Navajo');
    AList.Add('Chichewa');
    AList.Add('Occitan');
    AList.Add('Ojibwa');
    AList.Add('Oromo');
    AList.Add('Oriya');
    AList.Add('Ossetian');
    AList.Add('Panjabi');
    AList.Add('Pali');
    AList.Add('Polish');
    AList.Add('Pushto');
    AList.Add('Portuguese');
    AList.Add('Brazilian Portuguese');
    AList.Add('Quechua');
    AList.Add('Raeto-Romance');
    AList.Add('Rundi');
    AList.Add('Romanian');
    AList.Add('Russian');
    AList.Add('Kinyarwanda');
    AList.Add('Sanskrit');
    AList.Add('Sardinian');
    AList.Add('Sindhi');
    AList.Add('Northern Sami');
    AList.Add('Sango');
    AList.Add('Sinhalese');
    AList.Add('Slovak');
    AList.Add('Slovenian');
    AList.Add('Samoan');
    AList.Add('Shona');
    AList.Add('Somali');
    AList.Add('Albanian');
    AList.Add('Serbian');
    AList.Add('Swati');
    AList.Add('Sotho, Southern');
    AList.Add('Sundanese');
    AList.Add('Swedish');
    AList.Add('Swahili');
    AList.Add('Tamil');
    AList.Add('Telugu');
    AList.Add('Tajik');
    AList.Add('Thai');
    AList.Add('Tigrinya');
    AList.Add('Turkmen');
    AList.Add('Tagalog');
    AList.Add('Tswana');
    AList.Add('Tonga');
    AList.Add('Turkish');
    AList.Add('Tsonga');
    AList.Add('Tatar');
    AList.Add('Twi');
    AList.Add('Tahitian');
    AList.Add('Uighur');
    AList.Add('Ukrainian');
    AList.Add('Urdu');
    AList.Add('Uzbek');
    AList.Add('Venda');
    AList.Add('Vietnamese');
    AList.Add('Volapuk');
    AList.Add('Walloon');
    AList.Add('Wolof');
    AList.Add('Xhosa');
    AList.Add('Yiddish');
    AList.Add('Yoruba');
    AList.Add('Zhuang');
    AList.Add('Chinese');
    AList.Add('Chinese (Simplified)');
    AList.Add('Chinese (Traditional)');
    AList.Add('Zulu');
  finally
    AList.EndUpdate;
  end;
end;

function GetLanguageCode(Index: Integer): string;
begin
  case Index+1 of
    1: Result:= 'aa';
    2: Result:= 'ab';
    3: Result:= 'ae';
    4: Result:= 'af';
    5: Result:= 'ak';
    6: Result:= 'am';
    7: Result:= 'an';
    8: Result:= 'ar';
    9: Result:= 'as';
    10: Result:= 'av';
    11: Result:= 'ay';
    12: Result:= 'az';
    13: Result:= 'ba';
    14: Result:= 'be';
    15: Result:= 'bg';
    16: Result:= 'bh';
    17: Result:= 'bi';
    18: Result:= 'bm';
    19: Result:= 'bn';
    20: Result:= 'bo';
    21: Result:= 'br';
    22: Result:= 'bs';
    23: Result:= 'ca';
    24: Result:= 'ce';
    25: Result:= 'ch';
    26: Result:= 'co';
    27: Result:= 'cr';
    28: Result:= 'cs';
    29: Result:= 'cv';
    30: Result:= 'cy';
    31: Result:= 'da';
    32: Result:= 'de';
    33: Result:= 'de_AT';
    34: Result:= 'de_CH';
    35: Result:= 'dv';
    36: Result:= 'dz';
    37: Result:= 'ee';
    38: Result:= 'el';
    39: Result:= 'en';
    40: Result:= 'en_AU';
    41: Result:= 'en_CA';
    42: Result:= 'en_GB';
    43: Result:= 'en_US';
    44: Result:= 'eo';
    45: Result:= 'es';
    46: Result:= 'et';
    47: Result:= 'eu';
    48: Result:= 'fa';
    49: Result:= 'ff';
    50: Result:= 'fi';
    51: Result:= 'fj';
    52: Result:= 'fo';
    53: Result:= 'fr';
    54: Result:= 'fr_BE';
    55: Result:= 'fy';
    56: Result:= 'ga';
    57: Result:= 'gd';
    58: Result:= 'gl';
    59: Result:= 'gn';
    60: Result:= 'gu';
    61: Result:= 'gv';
    62: Result:= 'ha';
    63: Result:= 'he';
    64: Result:= 'hi';
    65: Result:= 'ho';
    66: Result:= 'hr';
    67: Result:= 'ht';
    68: Result:= 'hu';
    69: Result:= 'hy';
    70: Result:= 'hz';
    71: Result:= 'ia';
    72: Result:= 'id';
    73: Result:= 'ie';
    74: Result:= 'ig';
    75: Result:= 'ii';
    76: Result:= 'ik';
    77: Result:= 'io';
    78: Result:= 'is';
    79: Result:= 'it';
    80: Result:= 'iu';
    81: Result:= 'ja';
    82: Result:= 'jv';
    83: Result:= 'ka';
    84: Result:= 'kg';
    85: Result:= 'ki';
    86: Result:= 'kj';
    87: Result:= 'kk';
    88: Result:= 'kl';
    89: Result:= 'km';
    90: Result:= 'kn';
    91: Result:= 'ko';
    92: Result:= 'kr';
    93: Result:= 'ks';
    94: Result:= 'ku';
    95: Result:= 'kw';
    96: Result:= 'kv';
    97: Result:= 'ky';
    98: Result:= 'la';
    99: Result:= 'lb';
    100: Result:= 'lg';
    101: Result:= 'li';
    102: Result:= 'ln';
    103: Result:= 'lo';
    104: Result:= 'lt';
    105: Result:= 'lu';
    106: Result:= 'lv';
    107: Result:= 'mg';
    108: Result:= 'mh';
    109: Result:= 'mi';
    110: Result:= 'mk';
    111: Result:= 'ml';
    112: Result:= 'mn';
    113: Result:= 'mo';
    114: Result:= 'mr';
    115: Result:= 'ms';
    116: Result:= 'mt';
    117: Result:= 'my';
    118: Result:= 'na';
    119: Result:= 'nb';
    120: Result:= 'nd';
    121: Result:= 'ne';
    122: Result:= 'ng';
    123: Result:= 'nl';
    124: Result:= 'nl_BE';
    125: Result:= 'nn';
    126: Result:= 'no';
    127: Result:= 'nr';
    128: Result:= 'nv';
    129: Result:= 'ny';
    130: Result:= 'oc';
    131: Result:= 'oj';
    132: Result:= 'om';
    133: Result:= 'or';
    134: Result:= 'os';
    135: Result:= 'pa';
    136: Result:= 'pi';
    137: Result:= 'pl';
    138: Result:= 'ps';
    139: Result:= 'pt';
    140: Result:= 'pt_BR';
    141: Result:= 'qu';
    142: Result:= 'rm';
    143: Result:= 'rn';
    144: Result:= 'ro';
    145: Result:= 'ru';
    146: Result:= 'rw';
    147: Result:= 'sa';
    148: Result:= 'sc';
    149: Result:= 'sd';
    150: Result:= 'se';
    151: Result:= 'sg';
    152: Result:= 'si';
    153: Result:= 'sk';
    154: Result:= 'sl';
    155: Result:= 'sm';
    156: Result:= 'sn';
    157: Result:= 'so';
    158: Result:= 'sq';
    159: Result:= 'sr';
    160: Result:= 'ss';
    161: Result:= 'st';
    162: Result:= 'su';
    163: Result:= 'sv';
    164: Result:= 'sw';
    165: Result:= 'ta';
    166: Result:= 'te';
    167: Result:= 'tg';
    168: Result:= 'th';
    169: Result:= 'ti';
    170: Result:= 'tk';
    171: Result:= 'tl';
    172: Result:= 'tn';
    173: Result:= 'to';
    174: Result:= 'tr';
    175: Result:= 'ts';
    176: Result:= 'tt';
    177: Result:= 'tw';
    178: Result:= 'ty';
    179: Result:= 'ug';
    180: Result:= 'uk';
    181: Result:= 'ur';
    182: Result:= 'uz';
    183: Result:= 've';
    184: Result:= 'vi';
    185: Result:= 'vo';
    186: Result:= 'wa';
    187: Result:= 'wo';
    188: Result:= 'xh';
    189: Result:= 'yi';
    190: Result:= 'yo';
    191: Result:= 'za';
    192: Result:= 'zh';
    193: Result:= 'zh-cn';
    194: Result:= 'zh-tw';
    195: Result:= 'zu';
    else
    Result:= '';
  end;
end;

end.
