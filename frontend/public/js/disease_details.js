document.addEventListener("DOMContentLoaded", async function () {
    const symptomDetailsContainer = document.getElementById("symptoms-list");
    const diseaseContainer = document.getElementById("disease-container"); 
    const backButton = document.getElementById("back-btn");

    const selectedSymptoms = new Set(JSON.parse(localStorage.getItem("selectedSymptoms")) || []);
    let diagnosisResults = JSON.parse(localStorage.getItem("diagnosisResults")) || [];
    const urlParams = new URLSearchParams(window.location.search);
    let chosenDisease = urlParams.get('disease');

    // Format disease name for better readability
    function formatDiseaseName(disease) {
        return disease.replace(/_/g, ' ').replace(/\b\w/g, char => char.toUpperCase());
    }

    // Fetch disease symptoms map from API
    let diseaseSymptomsMap = {};
    try {
        const response = await fetch('http://localhost/api/diseases');
        const diseasesData = await response.json();
        
        // Transform API data into the required format
        diseasesData.forEach(item => {
            // Handle nested symptoms array (flatten if needed)
            let symptoms = [];
            if (Array.isArray(item.symptoms) && item.symptoms.length > 0) {
                // If symptoms is an array of arrays, flatten it
                if (Array.isArray(item.symptoms[0])) {
                    symptoms = item.symptoms.flat();
                } else {
                    symptoms = item.symptoms;
                }
            }
            diseaseSymptomsMap[item.disease] = symptoms;
        });
        console.log("Disease symptoms loaded successfully");
        // console.log("Disease symptoms map:", diseaseSymptomsMap);
    } catch (error) {
        console.error("Error fetching disease data:", error);
        // Fallback to empty map if API fails
        diseaseSymptomsMap = {};
    }

    // // Handle chosen disease
    // if (!chosenDisease || chosenDisease === "null") {
    //     chosenDisease = "Unknown Disease";
    // } else {
    //     try {
    //         chosenDisease = JSON.parse(chosenDisease);
    //     } catch (e) {
    //         chosenDisease = "Unknown Disease";
    //     }
    // }

    if (diagnosisResults.length === 1 && typeof diagnosisResults[0] === "string") {
        chosenDisease = diagnosisResults[0];
        localStorage.setItem("chosenDisease", JSON.stringify(chosenDisease));
    }

    // Always get the full symptom list from the API data for the chosen disease
    if (chosenDisease !== "Unknown Disease" && diseaseSymptomsMap[chosenDisease]) {
        diagnosisResults = diseaseSymptomsMap[chosenDisease];
        console.log("Symptoms for selected disease:", diagnosisResults);
    }
    formatDiseaseName(chosenDisease);
    diseaseContainer.innerHTML = `<h2>Diagnosis: ${formatDiseaseName(chosenDisease)}</h2>`;

    // Sort symptoms: Selected symptoms first, then diagnosis suggestions
    const allSymptoms = Object.keys(symptomimgs);
    const diagnosisSymptoms = new Set(diagnosisResults);
    
    allSymptoms.sort((a, b) => {
        if (selectedSymptoms.has(b)) return 1;
        if (selectedSymptoms.has(a)) return -1;
        if (diagnosisSymptoms.has(b)) return 1;
        if (diagnosisSymptoms.has(a)) return -1;
        return 0;
    });

    // Display symptoms
    allSymptoms.forEach(symptom => {
        if (selectedSymptoms.has(symptom) || diagnosisSymptoms.has(symptom)) {
            const symptomDiv = document.createElement("div");
            symptomDiv.classList.add("symptom-detail");

            const formattedSymptom = formatDiseaseName(symptom);
            const symptomName = document.createElement("h3");
            symptomName.textContent = formattedSymptom;

            const imgContainer = document.createElement("div");
            imgContainer.classList.add("symptom-img-container");

            const img = document.createElement("img");
            img.src = symptomimgs[symptom] || "imgs/default.jpg";
            img.alt = formattedSymptom;
            img.classList.add("symptom-img");
            imgContainer.appendChild(img);

            if (selectedSymptoms.has(symptom)) {
                const checkmark = document.createElement("img");
                checkmark.src = "imgs/checkmark.png";
                checkmark.alt = "Selected";
                checkmark.classList.add("checkmark-icon");
                imgContainer.appendChild(checkmark);
            }

            if (diagnosisSymptoms.has(symptom) && !selectedSymptoms.has(symptom)) {
                const diagnosisLabel = document.createElement("span");
                diagnosisLabel.textContent = "Suggested by Diagnosis";
                diagnosisLabel.classList.add("diagnosis-label");
                symptomDiv.appendChild(diagnosisLabel);
            }

            symptomDiv.appendChild(symptomName);
            symptomDiv.appendChild(imgContainer);
            symptomDetailsContainer.appendChild(symptomDiv);
        }
    });

    // Back button event listener
    backButton.addEventListener("click", function () {
        window.location.href = "/possible_diseases";
    });
});

// Symptom images mapping
const symptomimgs = {
    abdominal_bloating: "imgs/abdominal_bloating.jpg",
    appetite_loss: "imgs/appetite_loss.jpg",
    belching: "imgs/belching.png",
    bleeding: "imgs/bleeding.jpg",
    blood_in_stool: "imgs/blood_in_stool.jpg",
    bloody_urine: "imgs/bloody_urine.jpg",
    chest_pain: "imgs/chest_pain.jpeg",
    chills: "imgs/chills.jpg",
    cough: "imgs/cough.png",
    coughing_up_blood: "imgs/coughing_up_blood.jpg",
    dark_urine: "imgs/dark_urine.jpg",
    diarrhea: "imgs/diarrhea.png",
    difficulty_breathing: "imgs/difficulty_breathing.jpg",
    difficulty_moving: "imgs/difficulty_moving.jpeg",
    difficulty_swallowing: "imgs/difficulty_swallowing.jpeg",
    dizziness: "imgs/dizziness.webp",
    fatigue: "imgs/fatigue.jpg",
    fever: "imgs/fever.jpg",
    foul_smelling_discharge: "imgs/foul_smelling_discharge.jpg",
    hallucinations: "imgs/hallucinations.jpeg",
    headache: "imgs/headache.avif",
    heart_palpitations: "imgs/heart_palpitations.webp",
    hydrophobia: "imgs/hydrophobia.jpg",
    increased_urination: "imgs/increased_urination.jpg",
    intolerance_of_fatty_foods: "imgs/intolerance_of_fatty_foods.webp",
    itching: "imgs/itching.avif",
    jaundice: "imgs/jaundice.jpeg",
    joint_pain: "imgs/joint_pain.png",
    lockjaw: "imgs/lockjaw.jpg",
    losing_consciousness: "imgs/losing_consciousness.avif",
    lower_back_pain: "imgs/lower_back_pain.jpg",
    lymph_nodes: "imgs/lymph_nodes.avif",
    malformed_big_toes: "imgs/malformed_big_toes.jpg",
    muscle_aches: "imgs/muscle_aches.avif",
    muscle_pain: "imgs/muscle_pain.avif",
    muscle_spasms: "imgs/muscle_spasms.png",
    muscle_stiffness: "imgs/muscle_stiffness.jpg",
    nausea: "imgs/nausea.avif",
    night_sweats: "imgs/night_sweats.webp",
    no_appetite: "imgs/appetite_loss.jpg",
    numbness: "imgs/numbness.jpg",
    open_sores: "imgs/open_sores.jpg",
    pain_and_swelling: "imgs/painful_swelling.avif",
    pain_between_shoulder_blades: "imgs/pain_between_shoulder_blades.jpg",
    pain_in_right_shoulder: "imgs/pain_in_right_shoulder.png",
    painful_swelling: "imgs/painful_swelling.avif",
    painful_urination: "imgs/painful_urination.png",
    paralysis: "imgs/paralysis.jpg",
    pelvic_pain: "imgs/pelvic_pain.jpg",
    persistent_bone_pain: "imgs/persistent_bone_pain.webp",
    progressive_joint_stiffness: "imgs/progressive_joint_stiffness.webp",
    rash: "imgs/rash.jpg",
    red_eyes: "imgs/red_eyes.jpg",
    restricted_jaw_movement: "imgs/restricted_jaw_movement.png",
    runny_nose: "imgs/runny_nose.webp",
    seizures: "imgs/seizures.jpg",
    severe_cough: "imgs/severe_cough.jpeg",
    shortness_of_breath: "imgs/shortness_of_breath.avif",
    skin_discoloration: "imgs/skin_discoloration.jpg",
    sneezing: "imgs/sneezing.jpg",
    sore_throat: "imgs/sore_throat.avif",
    stomach_cramps: "imgs/stomach_cramps.jpg",
    stomach_pain: "imgs/stomach_pain.jpg",
    sweating: "imgs/sweating.jpg",
    swelling_or_lump: "imgs/swelling_or_lump.webp",
    swollen_lymphs: "imgs/swollen_lymphs.avif",
    tiredness: "imgs/tiredness.avif",
    unexplained_weight_loss: "imgs/unexplained_weight_loss.jpeg",
    upset_stomach: "imgs/upset_stomach.jpg",
    vomiting: "imgs/vomiting.avif",
    weakness: "imgs/weakness.webp",
    weakened_bones: "imgs/weakened_bones.jpeg",
    weight_loss: "imgs/weight_loss.avif"
};
