with Ada.Unchecked_Deallocation;

package body LISTES is
   procedure Free is
	new Ada.Unchecked_Deallocation (Object => T_Cellule, Name => T_Liste);
   
   
   
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

end LISTES;