with Ada.Text_IO;		use Ada.Texte_IO;
with Ada.Strings; 		use Ada.Strings;
with Ada.Integer_Text_IO; 	use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; 	use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;	use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;		use Ada.Command_Line;
with Ada.Exceptions;		use Ada.Exceptions;

procedure Routeur_simple is
	Cache : Integer;
	type Tab_Politique is (FIFO, LRU, LFU);
	Politique: Tab_Politique;
	Statistique : Boolean;
	Table : String;
	Paquet : String;
	Resultat : String;
	Nb_cmd : Integer;
	
begin
	Cache := 10;
	Politique := FIFO;
	Statistique := True;
	Table := "table.txt";
	Paquet := "paquets.txt";
	Resultat := "resultats.txt";
	Nb_cmd := 1;
	while Nb_cmd <= Argument_Count loop
		case Argument(Nb_cmd) is
			when "-c" =>
				Nb_cmd := Nb_cmd + 1;
				Cache := Traiter_c(Argument(Nb_cmd), Cache);
			when "-p" =>  
				Nb_cmd := Nb_cmd + 1;
				Politique := Traiter_p(Argument(Nb_cmd), Politique);
			when "-s" => Statistique := True;
			when "-S" => Statistique := False;
			when "-t" =>
				Nb_cmd := Nb_cmd + 1;
				Table := Traiter_t(Argument(Nb_cmd), Table);
			when "-q" => 
				Nb_cmd := Nb_cmd + 1;
				Paquet := Traiter_q(Argument(Nb_cmd), Paquet);
			when others =>
		end case;
		Nb_cmd := Nb_cmd + 1;
	end loop;
end Routeur_simple;


function Traiter_c(Nb : in Integer, Arg : in String, Cache : in out String) return String
begin
	Cache := Integer'Value(String);
Exception
	Constraint_Error => Put("Erreur : incompréhension après commande -c");
end Traiter_c;
