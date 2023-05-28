concrete DrawSpanish of Draw =

  open SyntaxSpa, ParadigmsSpa, LexiconSpa, IrregSpa
  in {

lincat
  Command = Utt ;
  Object = CN ;
  ObjectRef = NP ;
  Shape = CN ;
  Colour = AP ;
  Size = AP ;
  Place = Adv ;

lin
  drawCommand object =
      mkUtt (mkImp (mkVP (mkV2 "dibujar") (mkNP a_Det object))) -- draw a circle
    | mkUtt (mkNP a_Det object)                              -- a circle
    | mkUtt object                                           -- circle
    ;
  removeCommand object =
      mkUtt (mkImp (mkVP (mkV2 "eliminar") object)) ; 
  moveCommand object place =
      mkUtt (mkImp (mkVP (mkVP (mkV2 "mover") object) place)) ; --if we place the verb in the imperative we would have mueve
      
  shapeObject size colour shape = mkCN size (mkCN colour shape) ;

  theObjectRef object = mkNP the_Det object ;
  itObjectRef = it_NP ;
  
  circle_Shape = mkCN (mkN "círculo") ;
  square_Shape = mkCN (mkN "cuadrado") ;

  big_Size = mkAP big_A ;
  small_Size = mkAP small_A ;
  noSize = mkAP (mkA "") ; ---

  green_Colour = mkAP green_A ;
  red_Colour = mkAP red_A ;
  blue_Colour = mkAP blue_A ;
  yellow_Colour = mkAP yellow_A ;

  noColour = mkAP (mkA "") ; ---

  upPlace = pmkAdv "arriba" ;
  downPlace = pmkAdv "abajo" ;
  leftPlace = pmkAdv "izquierda" ;
  rightPlace = pmkAdv "a la derecha" ;
  midPlace = pmkAdv "al medio" ;
  
  noPlace = pmkAdv "" ;

oper
  pmkAdv : Str -> Adv = ParadigmsSpa.mkAdv ;
  }