let selectedSymptoms = [];
const maxSymptoms = 5;
let allSymptoms = []; // Store all symptoms for filtering

// Helper function to format symptom names consistently
function formatSymptomName(symptom) {
    return symptom
        .replace(/_/g, ' ')
        .replace(/\b\w/g, char => char.toUpperCase());
}

// Helper function to get image path with fallback
function getSymptomImagePath(symptom) {
    return `/imgs/symptoms/${symptom}.webp`;
}

async function fetchSymptoms() {
    try {
        const response = await fetch('http://localhost/api/symptoms');
        const symptoms = await response.json();
        
        allSymptoms = symptoms;
        
        await preloadImages(symptoms);
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
    
    // Create document fragment for better performance
    const fragment = document.createDocumentFragment();
    
    symptoms.forEach(symptom => {
        const div = document.createElement("div");
        div.classList.add("symptom");
        
        // Create text node for the formatted symptom name
        const textNode = document.createElement("div");
        textNode.textContent = formatSymptomName(symptom);
        textNode.classList.add("symptom-name");
        
        // Add image (already preloaded)
        const img = document.createElement("img");
        img.src = getSymptomImagePath(symptom);
        img.onerror = function() {
            this.src = '/imgs/symptoms/default.webp';
        };
        img.alt = formatSymptomName(symptom);
        img.classList.add("symptom-img");
        
        // Append elements in correct order
        div.appendChild(textNode);
        div.appendChild(img);

        div.addEventListener("click", function() {
            if (selectedSymptoms.length < maxSymptoms && !selectedSymptoms.includes(symptom)) {
                selectedSymptoms.push(symptom);
                updateSelectedSymptoms();
            }
        });

        fragment.appendChild(div);
    });
    
    container.appendChild(fragment);
}

// Search functionality with debouncing
function setupSearch() {
    const searchInput = document.getElementById("search-input");
    let debounceTimer;
    
    searchInput.addEventListener("input", function() {
        clearTimeout(debounceTimer);
        debounceTimer = setTimeout(() => {
            const searchTerm = this.value.toLowerCase();
                    
            // Filter symptoms based on search term
            const filteredSymptoms = allSymptoms.filter(symptom => {
                const formattedSymptom = symptom.replace(/_/g, ' ');
                return formattedSymptom.toLowerCase().includes(searchTerm);
            });
            
            // Display filtered symptoms
            displaySymptoms(filteredSymptoms);
        }, 300); // 300ms debounce delay
    });
}

// Helper function to preload images
function preloadImages(symptoms) {
    return new Promise((resolve) => {
        let loadedCount = 0;
        const totalImages = symptoms.length;
        
        // If there are no symptoms, resolve immediately
        if (totalImages === 0) {
            resolve();
            return;
        }
        
        // Preload each image
        symptoms.forEach(symptom => {
            const img = new Image();
            
            img.onload = img.onerror = () => {
                loadedCount++;
                
                // If all images are loaded, resolve the promise
                if (loadedCount === totalImages) {
                    resolve();
                }
            };
            img.src = getSymptomImagePath(symptom);
        });
    });
}

function updateSelectedSymptoms() {
    const container = document.getElementById("selected-symptoms");
    const fragment = document.createDocumentFragment();
    
    container.innerHTML = "";

    selectedSymptoms.forEach(symptom => {
        const div = document.createElement("div");
        div.classList.add("selected-symptom");

        div.textContent = formatSymptomName(symptom);

        const img = document.createElement("img");
        img.src = getSymptomImagePath(symptom);
        img.onerror = function() {
            this.src = '/imgs/symptoms/default.webp';
        };
        img.alt = symptom;
        div.appendChild(img);

        div.addEventListener("click", function() {
            selectedSymptoms = selectedSymptoms.filter(s => s !== symptom);
            updateSelectedSymptoms();
        });

        fragment.appendChild(div);
    });

    container.appendChild(fragment);
    document.getElementById("count").textContent = `${selectedSymptoms.length}/5`;
}

async function handleDiagnosisSubmission() {
    if (selectedSymptoms.length === 0) {
        alert("Please select at least one symptom!");
        return;
    }

    try {
        const response = await fetch(`http://localhost/api/diagnose?symptoms=${selectedSymptoms.join(",")}`);
        const diseases = await response.json();

        localStorage.setItem("selectedSymptoms", JSON.stringify(selectedSymptoms));
        localStorage.setItem("diagnosisResults", JSON.stringify(diseases));

        window.location.href = "/possible_diseases";
    } catch (error) {
        console.error("Error fetching diagnosis results:", error);
        alert("Failed to fetch diagnosis results. Please try again later.");
    }
}

// Initialize the app
document.addEventListener("DOMContentLoaded", function() {
    fetchSymptoms();
    setupSearch();
    
    document.getElementById("clear-btn").addEventListener("click", function() {
        selectedSymptoms = [];
        updateSelectedSymptoms();
    });
    
    document.getElementById("submit-btn").addEventListener("click", handleDiagnosisSubmission);
});