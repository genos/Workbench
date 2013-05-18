select pet.id, pet.name, pet.age, pet.dead
       from pet, person_pet, person
       where
       pet.id = person_pet.pet_id and
       person_pet.person_id = person.id and
       person.first_name = "Zed";