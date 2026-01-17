with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
--  with Ada.Integer_Text_IO;    use Ada.Integer_Text_IO;


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


   procedure association_liste(Liste: in T_Liste; Adresse_IP : in T_Adresse_IP; Association : in out Integer; Int : out T_interface; Adresse : out T_Liste; Masque : out T_Adresse_IP) is
      Actuelle : T_Liste;
      Masque_nouv : T_Adresse_IP;
   begin
      Actuelle := Liste;
      Rien_Interface (Int);
      Rien(Masque_nouv);
      while Actuelle /= Null loop
         if (Et(Adresse_IP,Actuelle.Masque) = Actuelle.Destination) and inf(Masque_nouv, Actuelle.Masque) then
            Association := Association + 1;
            Masque_nouv := Actuelle.Masque;
            Int := Actuelle.Int;
            Adresse := Actuelle;
            Masque := Actuelle.Masque;
         else
            Null;
         end if;
         Actuelle := Actuelle.Suivant;
      end loop;
      if Association /= 0 then
         Adresse.Frequence := Adresse.Frequence + 1;
      else 
         Null;
      end if;
   end association_liste;


   procedure Dest_Masq_Max(Routage : in T_Liste; Cellule : in T_Liste; Adresse_IP : in T_Adresse_IP; Masque : in out T_Adresse_IP; Destination : out T_Adresse_IP) is
      Actuelle : T_Liste;
      egalite : Boolean;
      bit : Integer;
   begin
      Actuelle := Routage;
      Destination := Et(Adresse_IP, Masque);
      while Actuelle /= null loop
         if (Et(Actuelle.Destination, Masque) = Cellule.Destination) and not(Cellule.Suivant = Actuelle.Suivant) then
            egalite := True;
            bit := 0;
            while egalite and bit < 32 loop
               bit := bit + 1;
               egalite := (Et(Produit(Actuelle.Destination, bit), Poids_Fort) = Et(Produit(Adresse_IP, bit), Poids_Fort));
            end loop;
            Rien(Masque);
            for i in 1..(bit + 1) loop
               Association_int(Masque, (32 - i));
            end loop;
            Destination := ET(Adresse_IP,Masque);
         else
            Null;
         end if;
         Actuelle := Actuelle.Suivant;
      end loop;
   end Dest_Masq_Max;


   function Taille(Liste : in T_Liste) return Integer is
      I : Integer;
      Actuelle : T_Liste;
   begin
      I := 0;
      Actuelle := Liste;
      while Actuelle /= Null loop
         I := I + 1;
         Actuelle := Actuelle.Suivant;
      end loop;
      return I;
   end Taille;


   procedure Ordre(Cache : in out T_Liste; Cellule : in out T_Liste; politique : in Tab_Politique) is
      Actuelle : T_Liste;
   begin
      if Politique = LRU then
         Actuelle := Cache;
         if Cache = Cellule then
            Cache := Cache.Suivant;
         else
            while Actuelle.Suivant /= Cellule loop
               Actuelle := Actuelle.Suivant;
            end loop;
            Actuelle.Suivant := Actuelle.Suivant.Suivant;
         end if;
         Ajout_routeur (Cache, Cellule.Destination, Cellule.Masque, Cellule.Int);
         Free(Cellule);
      else
         null;
      end if;
   end Ordre;

   procedure Elimination_FIFO(Liste : in out T_Liste) is 
      Cellule : T_Liste;
   begin
      Cellule := Liste;
      Liste := Liste.Suivant;
      Free(Cellule);
   end Elimination_FIFO;


   procedure Elimination_LRU(Liste : in out T_Liste) is
      Actuelle : T_Liste;
   begin
      Actuelle := Liste;
      if Actuelle = NUll then
         Null;
      elsif Actuelle.Suivant = Null then
         Liste := Liste.Suivant;
         Free(Actuelle);
      else
         while Actuelle.Suivant.Suivant /= NUll loop
            Actuelle := Actuelle.Suivant;
         end loop;
         Free(Actuelle.Suivant);
         Actuelle.Suivant := Null;
      end if;
   end Elimination_LRU;


   procedure Elimination_LFU(Liste : in out T_Liste) is
      Actuelle_prec : T_Liste;
      Actuelle : T_Liste;
      Min : Integer;
      Cellule : T_Liste;
      Cellule_prec : T_Liste;
   begin
      Min := Integer'Last;
      Actuelle := Liste;
      Actuelle_prec := Null;

      while Actuelle /= Null loop
         if Actuelle.Frequence < Min then
            Min := Actuelle.Frequence;
            Cellule := Actuelle;
            Cellule_prec := Actuelle_prec;
         else
            Null;
         end if;
         Actuelle_prec := Actuelle;
         Actuelle := Actuelle.suivant;
      end loop;

      if Cellule_prec = Null then
         Liste := Liste.Suivant;
      else
         Cellule_prec.Suivant := Cellule.Suivant;
      end if;
      Free(Cellule);
   end Elimination_LFU;


   procedure Elimination(Liste: in out T_Liste; politique : in Tab_Politique) is
   begin
      if Politique = FIFO then
         Elimination_FIFO(Liste);
      elsif Politique = LRU then
         Elimination_LRU(Liste);
      else
         Elimination_LFU(Liste);
      end if;
   end Elimination;



   procedure Afficher_Liste(Liste : in T_Liste) is
      Actuelle : T_Liste;
   begin
      Actuelle := Liste;
      while Actuelle /= Null loop
         Put("-->[Destination : ");
         Afficher_Adresse_IP (Actuelle.Destination);
         Put(" , Masque : ");
         Afficher_Adresse_IP (Actuelle.Masque);
         Put(" , Interface : ");
         Afficher_Int(Actuelle.Int);
         Put("]");
         New_Line;
         Actuelle := Actuelle.Suivant;
      end loop;
      Put("--E");
   end Afficher_Liste;


   procedure Ajout_routeur(Liste : in out T_Liste; Destination : in T_Adresse_IP; Masque : in T_Adresse_IP; Int : T_interface) is
   begin
      if liste = Null then
			liste := new T_Cellule;
			liste.Suivant := Null;
         liste.Frequence := 1;
         liste.Destination := Destination;
         liste.Masque := Masque;
         liste.Int := Int;
		else
			Ajout_routeur(liste.Suivant, Destination, Masque, Int);
		end if;
   end Ajout_routeur;


   function Avoir_Int(Cellule : in T_Liste) return T_interface is
   begin
      return Cellule.Int;
   end Avoir_Int;

   function Avoir_Des(Cellule : in T_Liste) return T_Adresse_IP is
   begin
      return Cellule.Destination;
   end Avoir_Des;



   function Presence_fin(Liste : in T_Liste; Cellule : in T_Liste) return Boolean is
      Actuelle : T_Liste;
      Presence : Boolean;
   begin
      Actuelle := Liste;
      if Actuelle = Null then
         Presence := False;
      else
         while Actuelle.Suivant /= Null loop
            Actuelle := Actuelle.Suivant;
         end loop;
         if (Cellule.Destination = Actuelle.Destination) and (Cellule.Masque = Actuelle.Masque) and (Cellule.Int = Actuelle.Int) then
            Presence := True;
         else
            Presence := False;
         end if; 
      end if;
      return Presence;
   end Presence_fin;


   function Presence(Liste : in T_Liste; Cellule : in T_Liste) return Boolean is
      Actuelle : T_Liste;
      Presence : Boolean;
   begin
      Presence := False;
      Actuelle := Liste;
      while Actuelle /= Null loop
         if (Cellule.Destination = Actuelle.Destination) and (Cellule.Masque = Actuelle.Masque) and (Cellule.Int = Actuelle.Int) then
            Presence := True;
         else
            null;
         end if;
         Actuelle := Actuelle.Suivant;
      end loop;
      return Presence;
   end Presence;
end LISTES;