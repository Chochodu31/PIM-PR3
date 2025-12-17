with Ada.Strings;               use Ada.Strings;	
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;  use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Exceptions;            use Ada.Exceptions;
with lca;
use lca;



procedure id_ad_IP(Texte : in String; adresse_IP : out T_Adresse_IP) is
   type Tab_Octets is array (1..4) of T_Octet;
   Octets : Tab_Octets := (0, 0, 0, 0);
   indice_octet : Integer := 1;
   valeur_courante : Integer := 0;
   caractere : Character;
    
   -- Convertir un character a un entier. 
   function valeur_numerique(c : Character) return Integer is
   begin
      return Character'Pos(c) - Character'Pos('0');
   end valeur_numerique;
    
begin
   -- Etape 1: Decomposer adresse IP.

   -- Vérifier que la chaîne n'est pas vide.
   if Texte'Length = 0 then
      raise IP_Adresse_Unknown_Error;
   end if;
   -- Parcourir la chaine character par character.
   for i in 1..Texte'Length loop
      caractere := Texte(i);
         if caractere = '.' then
            -- Vérifier qu'on n'a pas déjà 4 octets.
            if indice_octet > 3 then
               raise IP_Adresse_Unknown_Error; 
            else 
               null;   
            end if;
            -- Vérifier que l'octet est dans la plage valide.
            if valeur_courante < 0 or valeur_courante > 255 then
               raise IP_Adresse_Unknown_Error;
            else
               null;   
            end if; 
            -- Stocker la valeur courante puis la reinisialiser. 
            octets(indice_octet) := T_Octet(valeur_courante);
            indice_octet := indice_octet + 1;
            valeur_courante := 0;
            -- Vérifier qu'il n'y a pas deux points consécutifs.
            if i = Texte(1) or Texte(i-1) = '.' then
               raise IP_Adresse_Unknown_Error;
            else
               null;    
            end if;
            
         elsif caractere in '0'..'9' then
            -- Ajouter le chiffre à la valeur courante.
            valeur_courante := valeur_courante * 10 + valeur_numerique(caractere);
            -- Vérifier immédiatement s'il y'a depassemant.
            if valeur_courante > 255 then
                raise IP_Adresse_Unknown_Error;
            end if;
            
         else
            -- Pour un caractère invalide
            raise IP_Adresse_Unknown_Error;
        end if;
   end loop;
    
   -- Après la boucle, vérifier qu'on a exactement 4 octets
   if indice_octet /= 4 then
      raise IP_Adresse_Unknown_Error;  -- Pas assez de points
   end if;
    
   -- Vérifier le dernier octet
   if valeur_courante < 0 or valeur_courante > 255 then
      raise IP_Adresse_Unknown_Error;
   end if;
    
   -- Stocker le dernier octet
   octets(4) := T_Octet(valeur_courante);

   -- Vérifier qu'il n'y a pas de point à la fin
   if Texte(Texte'Last) = '.' then
      raise IP_Adresse_Unknown_Error;
   end if;
    
   -- Etape 2: construire l'adresse IP à partir des octets.
   adresse_IP := 0;
   for i in 1..4 loop
      adresse_IP := adresse_IP * UN_OCTET + T_Adresse_IP(octets(i));
   end loop;
    
exception
   when others =>
      raise IP_Adresse_Unknown_Error;
end id_ad_IP;