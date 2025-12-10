-- identifier adresse IP.

procedure Composer_ad_IP(Texte : in String) is
   type T_Tableau_Octets is array (1..4) of T_Octet;
   octets : T_Tableau_Octets;
   indice_octet : Integer := 1;
   valeur_courante : Integer := 0;
   c : Character;
begin
   for i in 1..Texte.Taille loop
      c := Texte(i);
      if c = '.' then
         if indice_octet > 3 then
             raise IP_Adresse_Unknown_Error;
         end if;   
         if valeur_courante > 255 then
             raise IP_Adresse_Unknown_Error;
         end if;
         octets(indice_octet) := T_Octet(valeur_courante);
         indice_octet := indice_octet + 1;
         valeur_courante := 0;         
      elsif c in '0'..'9' then
         valeur_courante := valeur_courante * 10  + Natural(c);
      else
         raise IP_Adresse_Unknown_Error;
      end if;
   end loop;
   if indice_octet /= 4 then
      raise IP_Adresse_Unknown_Error;
   end if;
   if valeur_courante > 255 then
      raise IP_Adresse_Unknown_Error;
   end if;
end id_ad_IP;


function Creer_ad_IP(Texte : in String) return T_Adresse_IP is
   adresse_IP : T_Adresse_IP := 0;
begin
   Composer_ad_IP(Texte);
   for i in 1..4 loop
      adresse_IP := adresse_IP * UN_OCTET + T_Adresse_IP(octets(i));
   end loop;
   return adresse_IP;
end;