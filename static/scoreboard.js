// Scavenger Hunt settings
const totalEventDuration = 60 * 35;
let intervalToNextRefresh = 15;
let ongoing = false;

// Containers
let wrapper = document.getElementById("scoreboard");
let metadata = document.querySelector(".hunt-meta");

// api data requester
const getScores = scoreLoaderCallback => {
  // result looks like:
  // [{"_totalScore":1255,"_endpoints":
  // {"AdminArticleDetail":["GET","PUT"],
  // "AdminArticleList":["POST"]},"_teamName":"erik"}]
  return fetch("/api/scores")
    .then(response => response.json())
    .then(data => scoreLoaderCallback(data))
    .catch(ex => {
      console.error("scoring load failed", ex);
    });
};

// Render Empty State
const emptyState = () => {
  wrapper.innerHTML = "";
  const newText = document.createElement("div");
  newText.classList = "empty-state";
  newText.innerHTML = `
		<svg class="empty-state__icon" viewBox="0 0 24 24" width="48" height="48" stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round">
			<line x1="12" y1="2" x2="12" y2="6"></line>
			<line x1="12" y1="18" x2="12" y2="22"></line>
			<line x1="4.93" y1="4.93" x2="7.76" y2="7.76"></line>
			<line x1="16.24" y1="16.24" x2="19.07" y2="19.07"></line>
			<line x1="2" y1="12" x2="6" y2="12"></line>
			<line x1="18" y1="12" x2="22" y2="12"></line>
			<line x1="4.93" y1="19.07" x2="7.76" y2="16.24"></line>
			<line x1="16.24" y1="7.76" x2="19.07" y2="4.93"></line>
		</svg>
		<div style="margin-top: 8px;">Loading...</div>
	`;
  wrapper.appendChild(newText);
  setTimeout(() => {
    newText.remove();
  }, 500);
};

// Render Table of Results
const renderResults = data => {
  if (wrapper === null) {
    wrapper = document.getElementById("scoreboard");
  }
  emptyState();
  const tableClass = "table";
  let table = document.createElement("table");
  table.classList = tableClass;
  table.innerHTML = `
  <thead class="table__head">
    <tr class="table__head-row">
      <th class="table__head-cell u-text--center">Place</th>
      <th class="table__head-cell">Team</th>
      <th class="table__head-cell">Endpoints</th>
      <th class="table__head-cell">Methods</th>
      <th class="table__head-cell u-text--right">Score</th>
    </tr>
  </thead>
  <tbody>
  </tbody>
`;
  const title = document.createElement("div");
  title.classList = "headline";
  title.innerHTML = `<h4 class="headline__title"><small class="u-text--danger">HTTP Scavenger Hunt</small><br />Leaderboard <small class="u-text--secondary">
    </small></h4><span class="chip ${ongoing ? "chip--success" : "chip--secondary"}">${
    ongoing ? "in Progress" : "Completed"
  }</span>`;
  wrapper.appendChild(title);
  wrapper.appendChild(table);

  const sorter = R.compose(
    R.reverse,
    R.sortBy(R.prop("_totalScore"))
  );
  const sortedData = sorter(data);
  const makeListItem = (item, idx) => {
    const tableBody = table.querySelector("tbody");
    const place = idx + 1;
    let endpoints = [];
    let methods = new Set();
    for (const key of Object.keys(item._endpoints)) {
      endpoints.push(key);
      item._endpoints[key].forEach(elem => methods.add(elem));
    }
    endpoints = endpoints.join(", ");
    methods = Array.from(methods).join(", ");

    let tr = document.createElement("tr");
    tr.classList = "table__row";
    tr.innerHTML = `
        <td class="table__cell table__cell--place u-text--center"><span class="place">${place}</span></td>
      <td class="table__cell table__cell--name">${item._teamName}
    <br><small style="opacity: .4;">${endpoints}</small>
    <br><small style="opacity: .4;">${methods}</small>
    </td>
    <td class="table__cell"><small>${endpoints}</small></td>
    <td class="table__cell"><small>${methods}</small></td>
    <td class="table__cell u-text--right"><strong>${item._totalScore}</strong></td>`;

    if (idx == 0) {
      tr.querySelector(".place").classList.add("place--first");

      // if it's over, we'd like to
      if (!ongoing) {
        const firstPlaceCard = document.createElement("div");
        firstPlaceCard.classList = "winner";
        firstPlaceCard.innerHTML = `
          <div class="winner__image">
            <svg viewBox="0 0 24 24" stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round">
              <circle cx="12" cy="8" r="7"></circle>
              <polyline points="8.21 13.89 7 23 12 20 17 23 15.79 13.88"></polyline>
            </svg>
          </div>
          <div class="winner__content">
            <small class="winner__badge">winner</small>
            <h5 class="winner__title">${item._teamName} </h5>
            <div class="winner__info">
              <small class="winner__info-item"><strong>${endpoints}</strong></small>
              <small class="winner__info-item">Points: <strong>${item._totalScore}</strong></small>
            </div>
          </div>
        `;
        table.parentNode.insertBefore(firstPlaceCard, table);
      }
    } else if (idx == 1) {
      tr.querySelector(".place").classList.add("place--second");
    } else if (idx == 3) {
      tr.querySelector(".place").classList.add("place--third");
    }
    tableBody.appendChild(tr);
  };
  sortedData.forEach(makeListItem);
};

// scavenger hunt init
initiateEvent = () => {
  let totalTimeRemaining = totalEventDuration;
  ongoing = true;
  getScores(renderResults);
  if (metadata === null) {
    metadata = document.querySelector(".hunt-meta");
  }
  const newTotalTimeRemaining = document.createElement("div");
  newTotalTimeRemaining.classList = "time-remaining-container";
  newTotalTimeRemaining.innerHTML = `
    <div><span class="time-remaining">Time Remaining: </span><span id="totalTimeRemaing">${totalTimeRemaining}</span></div>
  `;
  metadata.appendChild(newTotalTimeRemaining);

  const timeToNextRefresh = document.createElement("div");
  timeToNextRefresh.classList = "time-to-refresh-container";
  timeToNextRefresh.innerHTML = `
    <div><span class="time-to-refresh">Next refresh: </span><span id="timeToRefresh">${intervalToNextRefresh}</span></div>
  `;
  metadata.appendChild(timeToNextRefresh);
  const timeRemainingSpan = document.querySelector("#totalTimeRemaing");
  const timeToRefreshSpan = document.querySelector("#timeToRefresh");

  let interval = setInterval(() => {
    totalTimeRemaining -= 1;
    intervalToNextRefresh -= 1;

    timeRemainingSpan.innerHTML = totalTimeRemaining;
    timeToRefreshSpan.innerHTML = intervalToNextRefresh;

    if (intervalToNextRefresh < 1) {
      intervalToNextRefresh = 15;
      getScores(renderResults);
    }
    if (totalTimeRemaining < 1) {
      ongoing = false;
      // final refresh, which will set the leader
      getScores(renderResults);
      clearInterval(interval);
      newTotalTimeRemaining.parentNode.removeChild(newTotalTimeRemaining);
      timeToNextRefresh.parentNode.removeChild(timeToNextRefresh);
    }
  }, 1000);
};

// create stop and start buttons
const startButton = document.getElementById("hunt-start");
startButton.addEventListener("click", event => {
  if (ongoing) {
    return;
  }
  // if it's not started, we'll start it over;
  initiateEvent();
});
