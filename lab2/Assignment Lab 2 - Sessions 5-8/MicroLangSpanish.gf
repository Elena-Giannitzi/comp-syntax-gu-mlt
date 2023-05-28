--# -path=.:../abstract
concrete MicroLangSpanish of MicroLang = open MicroResSpanish in {

-----------------------------------------------------
---------------- Grammar part -----------------------
-----------------------------------------------------

  lincat
  
    Utt = {s : Str} ;
    S  = {s : Str} ;
    VP = {verb : Verb ; compl : Gender => Number => Str} ;
    Comp = {s : Gender => Number => Str} ;
    AP = Adjective ;
    CN = Noun ;
    NP = {s : Case => Str ; a : Agreement ; g : Gender ; n : Number ; isPron : Bool} ;
    Prep = {s : Str} ;
    Pron = {s : Case => Str ; a : Agreement ; g : Gender ; n : Number} ;
    V = Verb ;
    A = Adjective ;
    N = Noun ;
    Adv = {s : Str} ;
    V2 = Verb2 ;
    Det = Determiner ;

  lin
  
    UttS s = s ;
    UttNP np = {s = np.s ! Acc} ;

    PredVPS np vp = {
       s = np.s ! Nom ++ vp.verb.s ! (VFin Present np.a) ++ vp.compl ! np.g ! np.n
    };
    
    UseV v = {
      verb = v ;
      compl = \\g,n => ""
      } ;

    ComplV2 v2 np = {
          verb = case np.isPron of {
              True => v2 ** {s = \\vf => v2.c ++ np.s ! Acc ++ v2.s ! vf} ;
              False => v2 } ;
          compl = case np.isPron of {
              True => \\g,n => [] ;
              False => \\g, n => v2.c ++ np.s ! Acc 
          }
      } ;
      
    UseComp comp = {
      verb = be_Verb ;    
      compl = comp.s
      } ;
      
    CompAP ap = ap ;
      
    AdvVP vp adv =
      vp ** {compl = \\g,n => vp.compl ! g ! n ++ adv.s} ;
      
    DetCN det cn = {
      s = table { c => det.s ! cn.g  ++  cn.s ! det.n } ;
      a = Agr det.n P3 ;
      g = cn.g ;
      n = det.n ;
      isPron = False
    } ;
      
    UsePron p = p ** {isPron=True};
   
            
    a_Det = {s = table {Masc => "un" ; Fem => "una"} ; n = Sg};
    aPl_Det = { s = table {Masc => "unos" ; Fem => "unas"} ; n = Pl} ;
    thePl_Det = { s = table { Masc => "el" ; Fem => "la" } ; n = Sg } ;
    the_Det = { s = table { Masc => "los" ; Fem => "las" } ; n = Pl } ;
    
    UseN n = n ;
    
    AdjCN ap cn = {
      s = table {n => cn.s ! n ++ ap.s ! cn.g ! n} ;
      g = cn.g
      } ;

    PositA a = a ;
    
    in_Prep = {s = "en"} ;
    on_Prep = {s = "en"} ;
    with_Prep = {s = "con"} ;
    for_Prep = {s = "por"} ;
    

    PrepNP prep np = {s = prep.s ++ np.s ! Acc} ;

    he_Pron = {
      s = table {Nom => "él" ; Acc => "lo"} ;
      a = Agr Sg P3 ;
      g = Masc ;
      n = Sg
    } ;

    she_Pron = {
      s = table {Nom => "ella" ; Acc => "la"} ;
      a = Agr Sg P3 ;
      g = Fem ;
      n = Sg
    } ;

    they_Pron = {
      s = table {Nom => "ellos" ; Acc => "los"} ;
      a = Agr Pl P3 ;
      g = Masc ;
      n = Pl
    } ;
    
    they_Fem_Pron = {
      s = table {Nom => "ellas" ; Acc => "las"} ;
      a = Agr Pl P3 ;
      g = Fem ;
      n = Pl
    } ;

-----------------------------------------------------
---------------- Lexicon part -----------------------
-----------------------------------------------------

lin already_Adv = mkAdv "ya" ;
lin animal_N = mkN "animal" ;
lin apple_N = mkN "manzana" ;
lin baby_N = mkN "bebé" ;
lin bad_A = mkA "malo" ;
lin beer_N = mkN "cerveza" ;
lin big_A = mkA "grande" ;
lin bike_N = mkN "bicicleta" ;
lin bird_N = mkN "pájaro" ;
lin black_A = mkA "negro" ;
lin blood_N = mkN "sangre" ;
lin blue_A = mkA "azul" ;
lin boat_N = mkN "bote" ;
lin book_N = mkN "libro" ;
lin boy_N = mkN "niño" ;
lin bread_N = mkN "pan" ;
lin break_V2 = mkV2 (mkV "romper") ;
lin buy_V2 = mkV2 (mkV "comprar") ;
lin car_N = mkN "coche" ;
lin cat_N = mkN "gato" ;
lin child_N = mkN "chico" ;
lin child_Fem_N = mkN "chica" ;
lin city_N = mkN "ciudad" ;
lin clean_A = mkA "limpio" ;
lin clever_A = mkA "inteligente" ;
lin cloud_N = mkN "nube" ;
lin cold_A = mkA "frío" ;
lin come_V = mkV "venir" ;
lin computer_N = mkN "computadora" ;
lin cow_N = mkN "vaca" ;
lin dirty_A = mkA "sucio" ;
lin dog_N = mkN "perro" ;
lin drink_V2 = mkV2 (mkV "beber") ;
lin eat_V2 = mkV2 (mkV "comer") ;
lin find_V2 = mkV2 (mkV "encontrar") ;
lin fire_N = mkN "fuego" ;
lin fish_N = mkN "pez" ;
lin flower_N = mkN "flor" ;
lin friend_N = mkN "amigo" ;
lin friend_Fem_N = mkN "amiga" ;
lin girl_N = mkN "niña" ;
lin good_A = mkA "bueno" ;
lin go_V = mkV "ir" ;
lin grammar_N = mkN "gramática" ;
lin green_A = mkA "verde" ;
lin heavy_A = mkA "pesado" ;
lin horse_N = mkN "caballo" ;
lin hot_A = mkA "caliente" ;
lin house_N = mkN "casa" ;
-- lin john_PN = mkPN "Juan" ;
lin jump_V = mkV "saltar" ;
lin kill_V2 = mkV2 (mkV "matar") ;
lin know_V2 = mkV2 (mkV "saber") ;
lin language_N = mkN "idioma" ;
lin live_V = mkV "vivir";
lin love_V2 = mkV2 (mkV "amar") ;
lin man_N = mkN "hombre" ;
lin milk_N = mkN "leche" ;
lin music_N = mkN "música" ;
lin new_A = mkA "nuevo" ;
lin now_Adv = mkAdv "ahora" ;
lin old_A = mkA "viejo" ;
-- lin paris_PN = mkPN "París" ;
lin play_V = mkV "tocar" ;
lin read_V2 = mkV2 (mkV "leer") ;
lin ready_A = mkA "listo" ;
lin red_A = mkA "rojo" ;
lin river_N = mkN "río" ;
lin run_V = mkV "correr" ;
lin sea_N = mkN "mar" ;
lin see_V2 = mkV2 (mkV "ver") ;
lin ship_N = mkN "barco" ;
lin sleep_V = mkV "dormir" ;
lin small_A = mkA "pequeño" ;
lin star_N = mkN "estrella" ;
lin swim_V = mkV "nadar" ;
lin teach_V2 = mkV2 (mkV "enseñar") ;
lin train_N = mkN "tren" ;
lin travel_V = mkV "viajar" ;
lin tree_N = mkN "árbol" ;
lin understand_V2 = mkV2 (mkV "entender") ;
lin wait_V2 = mkV2 (mkV "esperar") ;
lin walk_V = mkV "caminar" ;
lin warm_A = mkA "cálido" ;
lin water_N = mkN "agua" ;
lin white_A = mkA "blanco" ;
lin wine_N = mkN "vino" ;
lin woman_N = mkN "mujer" ;
lin yellow_A = mkA "amarillo" ;
lin young_A = mkA "joven" ;

-- ---------------------------
-- -- Paradigms part ---------
---------------------------

oper

   mkN = overload {
     mkN : Str -> Noun = smartNoun ;
     mkN : Str -> Str -> Gender -> Noun = mkNoun ;
     mkN : Gender -> Noun -> Noun = \g, n -> n ** {g = g} ;
     } ;

       mkA = overload {
     mkA : Str -> Adjective = smartAdjective ;
     mkA : (msg,fsg,mpl,fpl : Str) -> Adjective = mkAdjective ;
     } ;
    
    mkV = overload {
      mkVerb : Str -> Verb
        = \s -> lin Verb (mkVerb s) ;
      mkVerb : Str -> Str -> Str -> Verb
        = \inf, pres, past -> lin Verb (irregVerb inf pres past) ;
    } ;

    mkV2 = overload {
      mkV2 : Str -> Verb2
        = \s -> lin Verb2 (mkVerb s ** {c = []}) ;
      mkV2 : Str -> Str -> Verb2
        = \s, p -> lin Verb2 (mkVerb s ** {c = p}) ;
      mkV2 : Verb -> Verb2
        = \v -> lin Verb2 (v ** {c = []}) ;
      mkV2 : Verb -> Str -> Verb2
        = \v, p -> lin Verb2 (v ** {c = p}) ;
    } ;

    mkAdv : Str -> Adv
        = \s -> lin Adv {s = s} ;
  
    mkPrep : Str -> Prep
      = \s -> lin Prep {s = s} ;

}