with Ada.Text_IO;            use Ada.Text_IO;
with SDA_Exceptions;         use SDA_Exceptions;
with Ada.Unchecked_Deallocation;

package body LCA is

	procedure Free is
		new Ada.Unchecked_Deallocation (Object => T_Cellule, Name => T_LCA);

	procedure Initialiser(Sda: out T_LCA) is
	begin
		Sda := Null;									-- On initialise la Sda à Null
	end Initialiser;


	procedure Detruire (Sda : in out T_LCA) is
	begin
		if Est_Vide(Sda) then
			Null;
		else
			Detruire(Sda.all.Suivant);					-- On appelle détruire sur la queue de liste
			Free(Sda);									-- On Free le pointeur qui pointe sur la tête
		end if;
	end Detruire;


	procedure Afficher_Debug (Sda : in T_LCA) is
	begin
		if Est_Vide(Sda) then
			Put("--E");
		else
			Put("-->[");
			Afficher_Cle(Sda.all.Cle);
			Put(" : ");
			Afficher_Donnee(Sda.all.Valeur);
			Put("]");
			Afficher_Debug(Sda.all.Suivant);
		end if;
	end Afficher_Debug;


	function Est_Vide (Sda : T_LCA) return Boolean is
	begin
		return (Sda = Null);							-- La liste vide correspond à un pointeur Null
	end;


	function Taille (Sda : in T_LCA) return Integer is
	begin
		if Est_Vide(Sda) then
			return 0;									-- La liste vide est de taille égale à 0
		else
			return 1 + Taille(Sda.all.Suivant);			-- On ajoute 1 à la taille à chaque cellule
		end if;
	end Taille;


	procedure Enregistrer (Sda : in out T_LCA ; Cle : in T_Cle ; Valeur : in T_Valeur) is
	begin
		if Est_Vide(Sda) then
			Sda := new T_Cellule;						-- On crée une nouvelle cellule contenant les valeurs voulues
			Sda.all.Cle := Cle;
			Sda.all.Valeur := Valeur;
			Sda.all.Suivant := Null;
		elsif (Sda.all.Cle = Cle) then
			Sda.all.Valeur := Valeur;					-- On modifie la valeur indexée par la clé `Cle`
		else
			Enregistrer(Sda.all.Suivant, Cle, Valeur);
		end if;
	end Enregistrer;


	function Cle_Presente (Sda : in T_LCA ; Cle : in T_Cle) return Boolean is
	begin
		if Est_Vide(Sda) then
			return False;
		else
			return (Sda.all.Cle = Cle) or Cle_Presente(Sda.all.Suivant, Cle);
		end if;
	end;


	function La_Valeur (Sda : in T_LCA ; Cle : in T_Cle) return T_Valeur is
	begin
		if Est_Vide(Sda) then
			raise Cle_Absente_Exception;
		else
			if (Sda.all.Cle = Cle) then
				return Sda.all.Valeur;					-- On retourne la valeur associée à la clé `Cle`
			else
				return La_Valeur(Sda.all.Suivant, Cle);
			end if;
		end if;
	end La_Valeur;


	procedure Supprimer (Sda : in out T_LCA ; Cle : in T_Cle) is
		Tmp_Sda : T_LCA;
	begin
		if Est_Vide(Sda) then
			raise Cle_Absente_Exception;
		elsif Est_Vide(Sda.all.Suivant) then
			if (Sda.all.Cle = Cle) then
				Detruire(Sda);
			else
				raise Cle_Absente_Exception;
			end if;
		else
			if (Sda.all.Suivant.all.Cle = Cle) then
				Tmp_Sda := Sda.all.Suivant; 			-- On stocke la case à supprimer dans un pointeur temporaire
				Sda.all.Suivant := Sda.all.Suivant.all.Suivant;
				Free (Tmp_Sda); 						-- On détruit le pointeur temporaire
			elsif (Sda.all.Cle = Cle) then
				Tmp_Sda := Sda; 						-- On stocke la case à supprimer dans un pointeur temporaire
				Sda := Sda.all.Suivant;
				Free (Tmp_Sda);							-- On détruit le pointeur temporaire
			else
				Supprimer(Sda.all.Suivant, Cle);
			end if;
		end if;
	end Supprimer;

	
	procedure Pour_Chaque (Sda : in T_LCA) is
	begin
		if Est_Vide(Sda) then
			Null;
		else
			begin
				Traiter(Sda.all.Cle, Sda.all.Valeur);	-- On applique le traitement à la tête de liste
				exception
					when others => Null;
			end;
			Pour_Chaque(Sda.all.Suivant);				-- On applique le traitement à la queue de liste
		end if;
	end Pour_Chaque;

end LCA;
