with Ada.Text_IO;            use Ada.Text_IO;
with SDA_Exceptions;         use SDA_Exceptions;
with Ada.Unchecked_Deallocation;

package body LCA is

	procedure Free is
		new Ada.Unchecked_Deallocation (Object => T_Cellule, Name => T_LCA);


	procedure Initialiser (Sda : out T_LCA) is
	begin
		Sda := Null;
	end Initialiser;


	function Est_Vide (Sda : T_LCA) return Boolean is
	begin
		return Sda = Null;
	end;


	procedure Detruire (Sda : in out T_LCA) is
		Nouv_Sda : T_LCA;
	begin
		while Sda /= Null loop
			Nouv_Sda := Sda;
			Sda := Sda.All.Suivant;
			Free (Nouv_Sda);
		end loop;
		Free (Sda);
	end Detruire;


	function Taille (Sda : in T_LCA) return Integer is
		Compteur : Integer;
		Curseur : T_LCA;
	begin
		Compteur := 0;
		Curseur := Sda;
		while Curseur /= Null loop
			Compteur := Compteur + 1;
			Curseur := Curseur.All.Suivant;
		end loop;
		return Compteur;
	end Taille;


	function Cle_Presente (Sda : in T_LCA ; Cle : in T_Cle) return Boolean is
		Curseur : T_LCA;
		Presence : Boolean;
        begin
		Curseur := Sda;
		Presence := False;
		while Curseur /= Null loop
			if Curseur.All.Cle = Cle then
				Presence := True;
			else
				Null;
			end if;
			Curseur := Curseur.All.Suivant;
		end loop;
		return Presence;
	end;


	function La_Valeur (Sda : in T_LCA ; Cle : in T_Cle) return T_Valeur is 
		Curseur : T_LCA;
		Val : T_Valeur;
	begin
		if Cle_Presente (Sda, Cle) then
			Curseur := Sda;
			while Curseur /= Null loop
				if Curseur.All.Cle = Cle then
					Val := Curseur.All.Valeur;
				else
					Null;
				end if;
				Curseur := Curseur.All.Suivant;
			end loop;
		else
			raise Cle_Absente_Error;
		end if;
		return Val;
	end La_Valeur;


	procedure Enregistrer (Sda : in out T_LCA ; Cle : in T_Cle ; Valeur : in T_Valeur) is
	begin
		Enregistrer_Recursif (Sda, Cle, Valeur);
	end Enregistrer;


	procedure Enregistrer_Iteratif (Sda : in out T_LCA ; Cle : in T_Cle ; Valeur : in T_Valeur) is 
		Curseur : T_LCA;
	begin
		Curseur := Sda;
		while Curseur /= Null and then Curseur.All.Cle /= Cle loop
			Curseur := Curseur.All.Suivant;
		end loop;

		if Curseur = Null then
			Curseur := new T_Cellule;
			Curseur.All.Suivant := Sda;
			Sda := Curseur;
			Curseur.All.Cle := Cle;
			Curseur.All.Valeur := Valeur;
		else
			Curseur.All.Valeur := Valeur;
		end if;
	end Enregistrer_Iteratif;


	procedure Enregistrer_Recursif (Sda : in out T_LCA ; Cle : in T_Cle ; Valeur : in T_Valeur) is
	begin
		if Sda = Null then
			Sda := new T_Cellule;
			Sda.All.Suivant := Null;
			Sda.All.Cle := Cle;
			Sda.All.Valeur := Valeur;
		elsif Sda.All.Cle = Cle then
			Sda.All.Valeur := Valeur;
		else
			Enregistrer_Recursif (Sda.All.Suivant, Cle, Valeur);
		end if;
	end Enregistrer_Recursif;


	procedure Supprimer (Sda : in out T_LCA ; Cle : in T_Cle) is
	begin
		Supprimer_Recursif (Sda, Cle);
	end Supprimer;


	procedure Supprimer_Iteratif (Sda : in out T_LCA ; Cle : in T_Cle) is
		Curseur : T_LCA;
		Curseur_prec : T_LCA;
	begin
		Curseur := Sda;
		Curseur_prec := Null;
		while Curseur /= Null and then Curseur.All.Cle /= Cle loop
			Curseur_prec := Curseur;
			Curseur := Curseur.All.Suivant;
		end loop;
		
		if Curseur = Null then
			raise Cle_Absente_Error;
		elsif Curseur_prec = Null then
			Curseur_prec := Sda;
			Sda := Sda.All.Suivant;
			Free (Curseur_prec);
		else
			Curseur_prec.All.Suivant := Curseur.All.Suivant;
			Free (Curseur);
		end if;
	end Supprimer_Iteratif;


   procedure Supprimer_Recursif (Sda : in out T_LCA ; Cle : in T_Cle) is
		Curseur : T_LCA;
	begin
		if Sda = Null then
			raise Cle_Absente_Error;
		elsif Sda.All.Cle = Cle then
			Curseur := Sda;
			Sda := Sda.All.Suivant;
			Free (Curseur);
		else
			Supprimer_Recursif (Sda.All.Suivant, Cle);
		end if;
	end Supprimer_Recursif;



	procedure Faire_Pour_Chaque (Sda : in T_LCA) is
		Compteur : T_LCA;
	begin
		Compteur := Sda;
		while Compteur /= Null loop
			begin
				Traiter (Compteur.All.Cle, Compteur.All.Valeur);
				Compteur := Compteur.All.Suivant;
			exception
				when others => Compteur := Compteur.All.Suivant;
			end;
		end loop;
	end Faire_Pour_Chaque;


	procedure Afficher_Debug (Sda : in T_LCA) is
		Compteur : T_LCA;
	begin
		Compteur := Sda;
		while Compteur /= Null loop
			Put ("-->[");
			Afficher_Cle (Compteur.All.Cle);
			Put (" : ");
			Afficher_Donnee (Compteur.All.Valeur);
			Put ("]");
			Compteur := Compteur.All.Suivant;
         New_Line;
		end loop;
		Put ("--E");
	end Afficher_Debug;


end LCA;
