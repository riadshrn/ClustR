# ClustR  
### Clustering de Variables : K-means – Qualitatif – Deep Learning  
**Package R + Application Shiny**

---

## 📌 Présentation

**ClustR** est un package R dédié au **clustering de variables**, permettant de regrouper des variables similaires pour :

- réduire la dimension,
- créer des variables synthétiques,
- améliorer l’interprétation,
- faciliter les modèles prédictifs.

Il intègre **trois algorithmes complémentaires** :

1. **ClustKMeansVar** — PCA + K-means réallocatif  
2. **ClustQualiVarclus** — MCA + rapport de corrélation η²  
3. **ClustDeepVar** — Autoencodeur + clustering dans l’espace latent  

---

# 🚀 1. ClustKMeansVar (K-means réallocatif)

Méthode inspirée de Vigneau & Qannari (2003) :contentReference[oaicite:1]{index=1}.

## 🔹 Principe général  
Les variables sont regroupées selon leur corrélation avec la **synthetic variable** (PC1) du cluster.

Pour chaque cluster \(C_g\), la synthetic variable est :

y_g = X_{C_g} * w_g


avec :

\[
w_g = \arg\max_{\|w\|=1} w^\top S_g w
\]

où \(S_g = X_{C_g}^\top X_{C_g}\).

## 🔹 Distance variable–cluster  
\[
d(j,g) = 1 - \rho(X_j , y_g)^2
\]

La variable \(X_j\) est affectée au cluster minimisant \(d(j,g)\).

## 🔹 Critère global optimisé  
\[
Q = \frac{B}{T}, \quad T = W + B
\]

où :

- \(W\) = inertie intra-cluster  
- \(B\) = inertie inter-clusters  

💡 **Plus Q est proche de 1, meilleure est la partition.**

## 🔹 Algorithme complet  
1. Standardiser les variables  
2. Initialiser une partition  
3. Calculer PC1 de chaque cluster  
4. Réaffecter chaque variable au cluster optimisant \(d(j,g)\)  
5. Mettre à jour Q  
6. Répéter jusqu’à convergence  

---

# 🚀 2. ClustQualiVarclus (Clustering qualitatif par MCA + η²)

Méthode dédiée aux variables catégorielles basée sur l’ACM :contentReference[oaicite:2]{index=2}.

## 🔹 Étape 1 — Encodage disjonctif complet  
Chaque variable catégorielle devient des indicatrices (one-hot) :

\[
X \to Z \in \{0,1\}^{n \times m}
\]

## 🔹 Étape 2 — MCA par cluster  
On réalise une analyse des correspondances multiples sur les modalités du cluster.

L’axe principal obtenu est \(Y_g\).

## 🔹 Étape 3 — Rapport de corrélation η²  
Pour une variable \(V\) et un axe factoriel \(Y_g\) :

\[
\eta^2(V, Y_g)
= \frac{\mathrm{Var}\left(\mathbb{E}[Y_g \mid V]\right)}{\mathrm{Var}(Y_g)}
\]

**Affectation :**

\[
V \in C_g \quad \Longleftrightarrow \quad g = \arg\max_{h} \eta^2(V, Y_h)
\]

## 🔹 Algorithme complet  
1. Partition initiale des variables  
2. MCA pour chaque cluster  
3. Calcul de \(\eta^2(V,Y_g)\) pour chaque variable  
4. Réaffectation selon l’η² maximal  
5. Répéter jusqu’à convergence  

## 🔹 Atouts  
- Idéal pour variables nominales / ordinales  
- Basé sur la géométrie du χ²  
- Interprétation claire via l’ACM  

---

# 🚀 3. ClustDeepVar (Autoencodeur + clustering latent)

Algorithme deep learning pour capturer les **relations non linéaires** entre variables :contentReference[oaicite:3]{index=3}.

## 🔹 Étape 1 — Standardisation  
\[
X_{\text{std}} = \frac{X - \mu}{\sigma}
\]

## 🔹 Étape 2 — Transposition  
Chaque variable devient une “observation” :

\[
X^\top \in \mathbb{R}^{p \times n}
\]

## 🔹 Étape 3 — Encodeur (embeddings)  
\[
Z = f_{\text{enc}}(X^\top), \qquad Z \in \mathbb{R}^{p \times d}
\]

Chaque variable est représentée par un vecteur latent \(z_j \in \mathbb{R}^d\).

## 🔹 Étape 4 — Reconstruction  
\[
\hat{X}^\top = f_{\text{dec}}(Z)
\]

## 🔹 Projection de variables illustratives  
\[
z_{\text{illu}} =
\frac{\sum_j \rho(x_j, v) z_j}{\sum_j \rho(x_j, v)}
\]


## 🔹 Étape 5 — Clustering des embeddings  
\[
C = \text{k-means}(Z, k)
\]

## 🔹 Soft clustering  
\[
p_{jk} = 
\frac{
\exp(-\|z_j - \mu_k\|^2)
}{
\sum_{\ell} \exp(-\|z_j - \mu_\ell\|^2)
}
\]

## 🔹 Projection de variables illustratives  

\[
z_{\text{illu}} =
\frac{\sum_j \rho(x_j, v) z_j}{\sum_j \rho(x_j, v)}
\]

---

# 📊 Fonctions principales de l’application Shiny

### ✔ Chargement de données  
- Jeux intégrés  
- Upload CSV / TSV / XLSX  
- Détection automatique des types et NA  

### ✔ Sélection des variables  
- Actives vs illustratives  
- Détection automatique des variables redondantes  
- Matrice de corrélation  

### ✔ Choix de l’algorithme  
- Détection automatique selon le type de données  
- Réglages :  
  - `n_clusters`  
  - `latent_dim`, `epochs`, `dropout` (deep)  

### ✔ Visualisations  
- PCA (KMeans)  
- MCA (Quali)  
- Embeddings 2D/3D (Deep)  
- Heatmaps, distances inter-clusters  
- η², inerties, silhouette  

### ✔ Nouvelles variables  
- Numériques : somme / moyenne / ratio / produit / max/min  
- Qualitatives : combinaison, ifelse factor, quantiles  
- Projection dans PCA, MCA ou latent space  
- Soft membership complet  

---

# 👥 Auteurs

- **Riad SAHRANE**  
- **Aya MECHERI**  
- **Thibaud LECOMTE**  
Encadrant : **Ricco Rakotomalala**

---

# 🏁 Statut du projet

- ✔ Package R complet  
- ✔ Application Shiny avancée  
- ✔ Visualisations interactives  
- ✔ Nouvelles variables (num & quali)  
- ✔ Documentation complète  

