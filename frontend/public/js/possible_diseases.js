document.addEventListener("DOMContentLoaded", function () {
  const diseaseListContainer = document.getElementById("disease-list");
  const backButton = document.getElementById("back-btn");
  
  // Retrieve diagnosis results from local storage
  const diagnosisResults = JSON.parse(localStorage.getItem("diagnosisResults")) || [];

  if (diagnosisResults.length === 0) {
      diseaseListContainer.innerHTML = "<p>No diseases detected.</p>";
      return;
  }

  diagnosisResults.forEach(disease => {
      const diseaseDiv = document.createElement("div");
      diseaseDiv.classList.add("disease-item");

      // Format disease name (snake_case to Sentence Case)
       const formattedDisease = disease.replace(/_/g, ' ').replace(/\b\w/g, char => char.toUpperCase());

      // Create image container
      const imgContainer = document.createElement("div");
      imgContainer.classList.add("img-container");

      // Create disease name overlay
      const overlayText = document.createElement("div");
      overlayText.classList.add("overlay-text");
      overlayText.textContent = formattedDisease;

      console.log(disease)

      // Create image element
      const img = document.createElement("img");
      img.src = `/imgs/diseases/${disease}.png`;
      img.alt = formattedDisease;
      img.classList.add("disease-img");

      // Create details button
      const detailsButton = document.createElement("button");
      detailsButton.textContent = "Details";
      detailsButton.classList.add("details-btn");
      detailsButton.addEventListener("click", function () {
          window.location.href = `/disease_details?disease=${disease}`;
      });

      // Append elements
      imgContainer.appendChild(overlayText);
      imgContainer.appendChild(img);
      diseaseDiv.appendChild(imgContainer);
      diseaseDiv.appendChild(detailsButton);
      diseaseListContainer.appendChild(diseaseDiv);
  });

  // Back button functionality
  backButton.addEventListener("click", function () {
    window.location.href = "/symptom_selection";
  });
});
