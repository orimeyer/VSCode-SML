git init hw_files
cd hw_files
git remote add -f origin https://github.com/OpenUniversity/PL-236319-Spr-2024.git
git config core.sparseCheckout true 
echo "Spr24/HW/Homework2/" > .git/info/sparse-checkout 
git pull origin master