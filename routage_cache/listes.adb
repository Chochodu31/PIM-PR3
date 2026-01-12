with Ada.Unchecked_Deallocation;

package body LISTES is
   procedure Free is
	new Ada.Unchecked_Deallocation (Object => T_Cellule, Name => T_Liste);
   
   type T_Octet is mod 2 ** 8;
   
   procedure Initialiser(liste : out T_Liste) is 
   begin
      liste := Null;
   end Initialiser;


   function Est_Vide(liste: in T_Liste) return Boolean is 
   begin
      return liste = Null;
   end Est_Vide;


	procedure Detruire (liste : in out T_Liste) is
		Nouv_liste : T_Liste;
	begin
		while liste /= Null loop
			Nouv_liste := liste;
			liste := liste.All.Suivant;
			Free (Nouv_liste);
		end loop;
		Free (liste);
	end Detruire;


   procedure Enregistrer_routage(liste : in out T_Liste; Frequence : Integer; Destination : T_Adresse_IP; Masque : T_Adresse_IP; Int : T_interface) is
   begin
      if liste = Null then
			liste := new T_Cellule;
			liste.Suivant := Null;
         liste.Frequence := Frequence;
         liste.Destination := Destination;
         liste.Masque := Masque;
         liste.Int := Int;
		else
			Enregistrer_routage(liste.All.Suivant, Frequence, Destination, Masque, Int);
		end if;
   end Enregistrer_routage;


   -- Afficher l'adresse IP.
   -- Exemple d'affichage : 
   -- 32.248.90.14
   procedure Afficher_Ad_IP(M1 : in T_Adresse_IP) is
   begin
      Put (Natural ((M1 / UN_OCTET ** 3) mod UN_OCTET), 1); 
      Put (".");
      Put (Natural ((M1 / UN_OCTET ** 2) mod UN_OCTET), 1); 
      Put (".");
      Put (Natural ((M1 / UN_OCTET ** 1) mod UN_OCTET), 1); 
      Put (".");
      Put (Natural  (M1 mod UN_OCTET), 1);
   end Afficher_Ad_IP;

end LISTES;