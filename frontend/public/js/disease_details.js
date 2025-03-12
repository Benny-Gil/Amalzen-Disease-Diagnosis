// Helper function to format disease/symptom names consistently
function formatName(name) {
    if (!name) return '';
    return name.replace(/_/g, ' ').replace(/\b\w/g, char => char.toUpperCase());
}

// Helper function to get image path with fallback - using same approach as symptoms_selection.js
function getSymptomImagePath(symptom) {    
    return `/imgs/symptoms/${symptom}.webp`;
}

// Fetch disease symptoms map from API
async function fetchDiseaseData() {
    try {
        const response = await fetch('http://localhost/api/diseases');
        const diseasesData = await response.json();
        
        // Transform API data into the required format
        const diseaseSymptomsMap = {};
        diseasesData.forEach(item => {
            // Handle nested symptoms array (flatten if needed)
            let symptoms = [];
            if (Array.isArray(item.symptoms) && item.symptoms.length > 0) {
                symptoms = Array.isArray(item.symptoms[0]) ? item.symptoms.flat() : item.symptoms;
            }
            diseaseSymptomsMap[item.disease] = symptoms;
        });
        
        console.log("Disease symptoms loaded successfully");
        return diseaseSymptomsMap;
    } catch (error) {
        console.error("Error fetching disease data:", error);
        return {}; // Return empty object as fallback
    }
}

// Preload symptom images to improve performance
function preloadImages(symptoms) {
    return new Promise((resolve) => {
        if (symptoms.length === 0) {
            resolve();
            return;
        }
        
        let loadedCount = 0;
        const totalImages = symptoms.length;
        
        symptoms.forEach(symptom => {
            const img = new Image();
            img.onload = img.onerror = () => {
                loadedCount++;
                if (loadedCount === totalImages) {
                    resolve();
                }
            };
            img.src = getSymptomImagePath(symptom);
            img.onerror = function() {
                this.src = '/imgs/symptoms/default.webp';
                loadedCount++;
                if (loadedCount === totalImages) {
                    resolve();
                }
            };
        });
    });
}

// Sort and display symptoms
function displaySymptoms(symptoms, selectedSymptoms, diagnosisSymptoms) {
    const symptomDetailsContainer = document.getElementById("symptoms-list");
    symptomDetailsContainer.innerHTML = '';
    
    // Sort symptoms: Selected symptoms first, then diagnosis suggestions
    symptoms.sort((a, b) => {
        if (selectedSymptoms.has(a) && !selectedSymptoms.has(b)) return -1;
        if (!selectedSymptoms.has(a) && selectedSymptoms.has(b)) return 1;
        if (diagnosisSymptoms.has(a) && !diagnosisSymptoms.has(b)) return -1;
        if (!diagnosisSymptoms.has(a) && diagnosisSymptoms.has(b)) return 1;
        return 0;
    });
    
    // Create document fragment for better performance
    const fragment = document.createDocumentFragment();

    // Display symptoms
    symptoms.forEach(symptom => {
        if (selectedSymptoms.has(symptom) || diagnosisSymptoms.has(symptom)) {
            const symptomDiv = document.createElement("div");
            symptomDiv.classList.add("symptom-detail");

            const formattedSymptom = formatName(symptom);
            const symptomName = document.createElement("h3");
            symptomName.textContent = formattedSymptom;

            const imgContainer = document.createElement("div");
            imgContainer.classList.add("symptom-img-container");

            const img = document.createElement("img");
            img.src = getSymptomImagePath(symptom);
            img.alt = formattedSymptom;
            img.classList.add("symptom-img");
            img.onerror = function() {
                this.src = '/imgs/symptoms/default.webp';;
            };
            imgContainer.appendChild(img);

            if (selectedSymptoms.has(symptom)) {
                const checkmark = document.createElement("img");
                checkmark.src = "/imgs/symptoms/checkmark.webp";
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
            fragment.appendChild(symptomDiv);
        }
    });
    
    symptomDetailsContainer.appendChild(fragment);
}

// Initialize the page
document.addEventListener("DOMContentLoaded", async function () {
    const diseaseContainer = document.getElementById("disease-container"); 
    const backButton = document.getElementById("back-btn");

    // Get stored data
    const selectedSymptoms = new Set(JSON.parse(localStorage.getItem("selectedSymptoms")) || []);
    let diagnosisResults = JSON.parse(localStorage.getItem("diagnosisResults")) || [];
    const urlParams = new URLSearchParams(window.location.search);
    let chosenDisease = urlParams.get('disease');

    // Fetch disease data
    const diseaseSymptomsMap = await fetchDiseaseData();

    // Handle single diagnosis result
    if (diagnosisResults.length === 1 && typeof diagnosisResults[0] === "string") {
        chosenDisease = diagnosisResults[0];
        localStorage.setItem("chosenDisease", JSON.stringify(chosenDisease));
    }

    // Get symptoms for the chosen disease
    if (chosenDisease !== "Unknown Disease" && diseaseSymptomsMap[chosenDisease]) {
        diagnosisResults = diseaseSymptomsMap[chosenDisease];
        console.log("Symptoms for selected disease:", diagnosisResults);
    }
    
    // Display disease name
    diseaseContainer.innerHTML = `<h2>Diagnosis: ${formatName(chosenDisease)}</h2>`;

    // Prepare data for display
    const diagnosisSymptoms = new Set(diagnosisResults);
    
    // Get all unique symptoms to display
    const symptomsToDisplay = [...new Set([
        ...Array.from(selectedSymptoms),
        ...Array.from(diagnosisSymptoms)
    ])];
    
    // Preload images before displaying
    await preloadImages(symptomsToDisplay);
    
    // Display symptoms
    displaySymptoms(symptomsToDisplay, selectedSymptoms, diagnosisSymptoms);

    // Back button event listener
    backButton.addEventListener("click", function () {
        window.location.href = "/possible_diseases";
    });
});