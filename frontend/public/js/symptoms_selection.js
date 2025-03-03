let selectedSymptoms = [];
const maxSymptoms = 5;
let allSymptoms = []; // Store all symptoms for filtering

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

async function fetchSymptoms() {
    try {
        // Show a loading indicator
        const container = document.getElementById("symptoms-list");
        container.innerHTML = '<div class="loader">Loading symptoms...</div>';
        
        // 1. Fetch all symptoms first
        const response = await fetch('http://localhost/api/symptoms');
        const symptoms = await response.json();
        
        // Store all symptoms for search functionality
        allSymptoms = symptoms;
        
        // 2. Preload all images before displaying anything
        await preloadImages(symptoms);
        
        // 3. Display symptoms
        displaySymptoms(symptoms);
        
    } catch (error) {
        console.error("Error fetching symptoms:", error);
        const container = document.getElementById("symptoms-list");
        container.innerHTML = '<div class="error">Failed to load symptoms. Please try again later.</div>';
    }
}

// Function to display symptoms
function displaySymptoms(symptoms) {
    const container = document.getElementById("symptoms-list");
    container.innerHTML = '';
    
    if (symptoms.length === 0) {
        container.innerHTML = '<div class="no-results">No symptoms match your search</div>';
        return;
    }
    
    symptoms.forEach(symptom => {
        const div = document.createElement("div");
        div.classList.add("symptom");
        
        // Convert snake_case to Sentence case
        const formattedSymptom = symptom
            .replace(/_/g, ' ')
            .replace(/\b\w/g, char => char.toUpperCase());
        
        // Create text node for the formatted symptom name
        const textNode = document.createElement("div");
        textNode.textContent = formattedSymptom;
        textNode.classList.add("symptom-name");
        
        // Add image (already preloaded)
        const img = document.createElement("img");
        img.src = symptomimgs[symptom] ? symptomimgs[symptom] : "imgs/default.jpg";
        img.alt = formattedSymptom;
        img.classList.add("symptom-img");
        
        // Append elements in correct order
        div.appendChild(textNode);
        div.appendChild(img);

        div.addEventListener("click", function () {
            if (selectedSymptoms.length < maxSymptoms && !selectedSymptoms.includes(symptom)) {
                selectedSymptoms.push(symptom);
                updateSelectedSymptoms();
            }
        });

        container.appendChild(div);
    });
}

// Search functionality
function setupSearch() {
    const searchInput = document.getElementById("search-input");
    
    searchInput.addEventListener("input", function() {
        const searchTerm = this.value.toLowerCase();
                
        // Filter symptoms based on search term
        const filteredSymptoms = allSymptoms.filter(symptom => {
            const formattedSymptom = symptom.replace(/_/g, ' ');
            return formattedSymptom.toLowerCase().includes(searchTerm);
        });
        
        console.log("Found matches:", filteredSymptoms.length);
        // Display filtered symptoms
        displaySymptoms(filteredSymptoms);
    });
}

// Helper function to preload images
function preloadImages(symptoms) {
    return new Promise((resolve) => {
        let loadedCount = 0;
        const totalImages = symptoms.length;
        const container = document.getElementById("symptoms-list");
        
        // Update the loading indicator with progress
        function updateProgress() {
            const percent = Math.round((loadedCount / totalImages) * 100);
            container.innerHTML = `<div class="loader">Loading images... ${percent}%</div>`;
        }
        
        // If there are no symptoms, resolve immediately
        if (totalImages === 0) {
            resolve();
            return;
        }
        
        // Preload each image
        symptoms.forEach(symptom => {
            const imgSrc = symptomimgs[symptom] ? symptomimgs[symptom] : "imgs/default.jpg";
            const img = new Image();
            
            img.onload = img.onerror = () => {
                loadedCount++;
                updateProgress();
                
                // If all images are loaded, resolve the promise
                if (loadedCount === totalImages) {
                    resolve();
                }
            };
            
            // Start loading the image
            img.src = imgSrc;
        });
    });
}

function updateSelectedSymptoms() {
    const container = document.getElementById("selected-symptoms");
    container.innerHTML = "";

    selectedSymptoms.forEach(symptom => {
        const div = document.createElement("div");
        div.classList.add("selected-symptom");

        const formattedSymptom = symptom
                .replace(/_/g, ' ')
                .replace(/\b\w/g, char => char.toUpperCase());
        div.textContent = formattedSymptom;

        const img = document.createElement("img");
        img.src = symptomimgs[symptom] || "frontend/imgs/default.jpg";
        img.alt = symptom;
        div.appendChild(img);

        div.addEventListener("click", function () {
            selectedSymptoms = selectedSymptoms.filter(s => s !== symptom);
            updateSelectedSymptoms();
        });

        container.appendChild(div);
    });

    document.getElementById("count").textContent = `${selectedSymptoms.length}/5`;
}

document.getElementById("clear-btn").addEventListener("click", function () {
    selectedSymptoms = [];
    updateSelectedSymptoms();
});

document.getElementById("submit-btn").addEventListener("click", async function () {
    if (selectedSymptoms.length === 0) return alert("Please select at least one symptom!");

    try {
        const response = await fetch(`http://localhost/api/diagnose?symptoms=${selectedSymptoms.join(",")}`);
        const diseases = await response.json();

        localStorage.setItem("selectedSymptoms", JSON.stringify(selectedSymptoms));
        localStorage.setItem("diagnosisResults", JSON.stringify(diseases));

        console.log("Selected Symptoms:", JSON.stringify(selectedSymptoms));
        console.log("Diagnosis Results:", JSON.stringify(diseases));

        window.location.href = "/possible_diseases";
    } catch (error) {
        console.error("Error fetching diagnosis results:", error);
        alert("Failed to fetch diagnosis results. Please try again later.");
    }
});

// Initialize the app
document.addEventListener("DOMContentLoaded", function() {
    fetchSymptoms();
    setupSearch();
    
    document.getElementById("clear-btn").addEventListener("click", function () {
        selectedSymptoms = [];
        updateSelectedSymptoms();
    });
    
    document.getElementById("submit-btn").addEventListener("click", async function () {
        if (selectedSymptoms.length === 0) return alert("Please select at least one symptom!");
    
        try {
            const response = await fetch(`http://localhost/api/diagnose?symptoms=${selectedSymptoms.join(",")}`);
            const diseases = await response.json();
    
            localStorage.setItem("selectedSymptoms", JSON.stringify(selectedSymptoms));
            localStorage.setItem("diagnosisResults", JSON.stringify(diseases));
    
            console.log("Selected Symptoms:", JSON.stringify(selectedSymptoms));
            console.log("Diagnosis Results:", JSON.stringify(diseases));
    
            window.location.href = "/possible_diseases";
        } catch (error) {
            console.error("Error fetching diagnosis results:", error);
            alert("Failed to fetch diagnosis results. Please try again later.");
        }
    });
});