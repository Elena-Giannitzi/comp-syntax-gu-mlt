resource MicroResSpanish = {

param
    -- define types of morphological parameters
    Number = Sg | Pl ;
    Gender = Masc | Fem ;
    Case = Nom | Acc ;
    Agreement = Agr Number Person ;
    Person = P1 | P2 | P3 ;
    Tense = Present | Past ;
    VForm = VInfin | VFin Tense Agreement ;
    Bool = True | False ; 

oper
    -- define types for parts of speech
    -- they are record types with tables and inherent features
    Noun : Type = {s : Number => Str ; g : Gender} ;
    Adjective : Type = {s : Gender => Number => Str} ;
    Verb : Type = {s : VForm => Str} ;
    Determiner : Type = {s : Gender => Str ; n: Number} ;

-- here is an example that is type-correct as a Noun
  mujer_N : Noun = {
    s = table {Sg => "mujer" ; Pl => "mujeres"} ;
    g = Fem
    } ;

-- define constructor function for Noun
   mkNoun : Str -> Str -> Gender -> Noun = \sg, pl, g -> {
     s = table {Sg => sg ; Pl => pl} ;
     g = g
     } ;

-- define a noun using this constructor
   uomo_N : Noun = mkNoun "hombre" "hombres" Masc ;

-- define a smart paradigm
   smartNoun : Str -> Noun = \s -> case s of {
     x + "o" => mkNoun s (x + "os") Masc ;
     x + "or" => mkNoun s (x + "es") Masc ;
     x + "aje" => mkNoun s (x + "ajes") Masc ;
     x + "e" => mkNoun s (x + "es") Masc ;
     x + "é" => mkNoun s (x + "es") Masc ;
     x + "men" => mkNoun s (x + "menes") Masc ;
     x + "u" => mkNoun s (x + "s") Masc ;
     x + "ú" => mkNoun s (x + "s") Masc ;
     x + "an" => mkNoun s (x + "anes") Masc ;
     x + "ez" => mkNoun s (x + "eces") Masc ;
     x + "al" => mkNoun s (x + "ales") Masc ;
     x + "a" => mkNoun s (x + "as") Fem ;
     x + "ad" => mkNoun s (x + "ades") Fem ;
     x + "ed" => mkNoun s (x + "edes") Fem ;
     x + "ud" => mkNoun s (x + "udes") Fem ;
     x + "ión" => mkNoun s (x + "iones") Fem ;
     x + "umbre" => mkNoun s (x + "umbres") Fem ;
     x + "ie" => mkNoun s (x + "ies") Fem ;
     x + "er" => mkNoun s (x + "eres") Fem ;
     "mar" => mkNoun s "mares" Masc ; 
     "tren" => mkNoun s "trenes" Masc ;
     "árbol" => mkNoun s "árboles" Masc ;
     _ => mkNoun s s Masc
     } ;


    -- THIS IS PART OF THE PARADIGMS

-- the overloaded paradigm is what the lexicon will use
--    mkN = overload {
--      mkN : Str -> Noun = smartNoun ;
--      mkN : Str -> Str -> Gender -> Noun = mkNoun ;
--      mkN : Gender -> Noun -> Noun = \g, n -> n ** {g = g} ;
--      } ;

-- adjectives:
   mkAdjective : (msg,fsg,mpl,fpl : Str) -> Adjective = \msg,fsg,mpl,fpl -> {
     s = table {
       Masc => table {Sg => msg ; Pl => mpl} ;
       Fem => table {Sg => fsg ; Pl => fpl}
       }
     } ;
   smartAdjective : Str -> Adjective = \s -> case s of {
     x + "o" => mkAdjective s (x + "a") (x + "os") (x + "as") ; 
     x + "e" => mkAdjective s s (x + "es") (x + "es");
     x + "ul" => mkAdjective s s (x + "ules") (x + "ules"); 
     x + "í" => mkAdjective s s (x + "es") (x + "es");
     x + "or" => mkAdjective s (x + "ora") (x + "es") (x + "oras");
     x + "ol" => mkAdjective s (x + "ola") (x + "es") (x + "olas");
     x + "és" => mkAdjective s (x + "esa") (x + "es") (x + "esas");
     x + "en" => mkAdjective s (x + "en") (x + "enes") (x + "enes");
     x + _ => mkAdjective s s (x + "ices") (x + "ices");
     _ => mkAdjective s s s s
     } ;
     
     -- THIS IS PART OF THE PARADIGMS

--    mkA = overload {
--      mkA : Str -> Adjective = smartAdjective ;
--      mkA : (msg,fsg,mpl,fpl : Str) -> Adjective = mkAdjective ;
--      } ;       
   
   -- starting with the verbs


    mkTense : (inf, salto, saltas, salta, saltamos, saltáis, saltan, salté, saltaste, saltó, saltamos, saltasteis, saltaron : Str) -> Verb
= \saltar, salto, saltas, salta, saltamos, saltáis, saltan, salté, saltaste, saltó, saltamos, saltasteis, saltaron -> {
  s = table {
    VInfin => saltar ;
    (VFin Present (Agr Sg P1)) => salto ;
    (VFin Present (Agr Sg P2)) => saltas;
    (VFin Present (Agr Sg P3)) => salta ;
    (VFin Present (Agr Pl P1)) => saltamos ;
    (VFin Present (Agr Pl P2)) => saltáis ;
    (VFin Present (Agr Pl P3)) => saltan ;
    (VFin Past (Agr Sg P1)) => salté ;
    (VFin Past (Agr Sg P2)) => saltaste ;
    (VFin Past (Agr Sg P3)) => saltó ;
    (VFin Past (Agr Pl P1)) => saltamos ;
    (VFin Past (Agr Pl P2)) => saltasteis ;
    (VFin Past (Agr Pl P3)) => saltaron 
  }
};

  mkVerb : Str -> Verb
    = \inf -> case inf of {
    "saber" => mkTense inf "sé" "sabes" "sabe" "sabemos" "sabéis" "saben" "supe" "supiste" "supo" "supimos" "supisteis" "supieron";
    "ver" => mkTense inf "veo" "ves" "ve" "vemos" "veis" "ven" "vi" "viste" "vio" "vimos" "visteis" "vieron";
    x + "er" => mkTense inf (x + "o") (x + "es") (x + "e") (x + "emos") (x + "éis") (x + "en") (x + "í") (x + "iste") (x + "ió") (x + "imos") (x + "isteis") (x + "ieron");
    "tocar" => mkTense inf "toco" "tocas" "toca" "tocamos" "tocáis" "tocan" "toqué" "tocaste" "tocó" "tocamos" "tocasteis" "tocaron";
    "venir" => mkTense inf "vengo" "vienes" "viene" "venimos" "venís" "vienen" "vine" "viniste" "vino" "vinimos" "vinisteis" "vinieron";
    "ir" => mkTense inf "voy" "vas" "va" "vamos" "vais" "van" "fui" "fuiste" "fue" "fuimos" "fuisteis" "fueron";
    "dormir" => mkTense inf "duermo" "duermes" "duerme" "dormimos" "dormís" "duermen" "dormí" "dormiste" "durmió" "dormimos" "dormisteis" "durmieron";
      x + "ar" => mkTense inf (x + "o") (x + "as") (x + "a") (x + "amos") (x + "áis") (x + "an") (x + "é") (x + "aste") (x + "ó") (x + "amos") (x + "asteis") (x + "aron") ;
      x + "ir" => mkTense inf (x + "o") (x + "es") (x + "e") (x + "imos") (x + "ís") (x + "en") (x + "í") (x + "iste") (x + "ió") (x + "imos") (x + "isteis") (x + "ieron") ;
      _ => mkTense inf inf inf inf inf inf inf inf inf inf inf inf inf
    };

Verb2 : Type = {s : VForm => Str ; c : Str} ;

be_Verb : Verb = mkTense "ser" "soy" "eres" "es" "somos" "sois" "son" "fui" "fuiste" "fue" "fuimos" "fuisteis" "fueron" ;

irregVerb : (inf, present, past : Str) -> Verb =
  \inf, present, past ->
    mkTense inf present present present present present present past past past past past past;

}